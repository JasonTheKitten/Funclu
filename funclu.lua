local ANY_SYMBOL = {}
local APPLY_SEQ_SYMBOL = {}
local ARGS_SYMBOL = {}
local ARG_SYMBOL = {}
local CUSTOM_TYPE_SYMBOL = {}
local EVAL_SYMBOL = {}
local EXTENDS_SYMBOL = {}
local INSTATIATED_TYPE_SYMBOL = {}
local METHOD_DEFINITION_SYMBOL = {}
local SEQ_SYMBOL = {}
local SEQ_DONE_SYMBOL = {}
local SKIP_EVAL_SYMBOL = {}
local UNRESOLVED_KEY_SYMBOL = {}

local ISSUES_URL = "https://github.com/JasonTheKitten/Funclu/issues"

--

local enableDebug = false

--

local function cloneTable(tbl, recursive)
  local newTbl = {}
  for k, v in pairs(tbl) do
    if recursive and type(v) == "table" then
      newTbl[k] = cloneTable(v, true)
    else
      newTbl[k] = v
    end
  end
  return newTbl
end

local function defaultTable(dest, src)
  for k, v in pairs(src) do
    if dest[k] == nil then
      dest[k] = v
    end
  end

  return dest
end

local nestedKeyFunc
nestedKeyFunc = function(func, key, index, keyName)
  local newKey = cloneTable(key)
  keyName = keyName .. (#newKey == 0 and "" or ".") .. index
  table.insert(newKey, index)

  return setmetatable({}, {
    __index = function(self, key)
      if (type(key) == "table") then
        if key == UNRESOLVED_KEY_SYMBOL then
          return self()
        end
        -- Possibly a symbol. Maybe report in the future?
        return
      end
      return nestedKeyFunc(func, newKey, key, keyName)
    end,
    __call = function(_, ...)
      return func(newKey, keyName, ...)
    end
  })
end
local function keyFunc(func)
  return setmetatable({}, {
    __index = function(_, key)
      return nestedKeyFunc(func, {}, key, "")
    end,
    __call = function(_, key, ...)
      return func({key}, key, ...)
    end
  })
end

--

local function eval(ctx, func, ...)
  local res = func
  if type(res) == "function" then
    local args = { ... }
    if #args > 0 then
      res = res(...)
    end
    res = res(EVAL_SYMBOL, ctx)
  end
  if (type(res) == "table") and res[ARG_SYMBOL] and not ctx.disableArgumentResolution then
    return ctx.args[res.name]
  end
  if (type(res) == "table") and res[SKIP_EVAL_SYMBOL] then
    return res.value
  end
  return res
end

local function evalAll(ctx, args)
  local eArgs = {}
  for i = 1, #args do
    eArgs[i] = eval(ctx, args[i])
  end
  return eArgs
end

local function applyArgs(ctx, args)
  local appliedArgs = {}
  for k, v in ipairs(args) do
    if type(v) == "table" and v[APPLY_SEQ_SYMBOL] then
      local seq = eval(ctx, v.seq)
      local i = 1
      while true do
        local value = seq.at(i)
        i = i + 1
        if value == SEQ_DONE_SYMBOL then
          break
        end
        table.insert(appliedArgs, value)
      end
    elseif type(v) == "table" and v[UNRESOLVED_KEY_SYMBOL] then
      table.insert(appliedArgs, v())
    else
      table.insert(appliedArgs, v)
    end
  end

  return appliedArgs
end

local funcify
local function evalFunction(ctx, func, eArgs, disableAutoEval, argsData)
  local completed =
    (type(argsData) == "number" and #eArgs >= argsData)
    or (type(argsData) ~= "number")

  if completed then
    local result, actual = func(ctx, table.unpack(eArgs))
    actual = actual or (type(argsData) == "number" and argsData) or #eArgs
    if type(result) == "function" then
      local remainingArgs = { select(actual + 1, table.unpack(eArgs)) }
      -- TODO: Check if below logic is right
      if #remainingArgs == 0 then
        return result(EVAL_SYMBOL, ctx)
      end
      return result(table.unpack(remainingArgs))(EVAL_SYMBOL, ctx)
    end
    return result
  end
  
  local remaining = argsData - #eArgs
  return funcify(function(ctx, ...)
    local margs = { ... }
    local allArgs = {}
    for k, v in ipairs(eArgs) do
      table.insert(allArgs, { [SKIP_EVAL_SYMBOL] = true, value = v })
    end
    for k, v in ipairs(margs) do
      table.insert(allArgs, v)
    end
    return evalFunction(ctx, func, evalAll(ctx, allArgs), remaining)
  end, remaining, disableAutoEval, ctx.debugRef[1])
end

funcify = function(func, argsData, disableAutoEval, extraCallSites)
  local f
  f = function(args, collectedCallSites)
    return function(...)
      local margs = { ... }
      if margs[1] == EVAL_SYMBOL then
        local ctx = margs[2]
        local eArgs = applyArgs(ctx, args)
        if not disableAutoEval then
          eArgs = evalAll(ctx, eArgs)
        end
        local oldCallSites = ctx.debugRef[1]
        ctx.debugRef[1] = collectedCallSites
        local result = evalFunction(ctx, func, eArgs, disableAutoEval, argsData)
        ctx.debugRef[1] = oldCallSites
        return result
      else
        local allArgs = {}
        for k, v in ipairs(args) do
          table.insert(allArgs, v)
        end
        for k, v in ipairs(margs) do
          table.insert(allArgs, v)
        end

        local mCollectedCallSites = cloneTable(collectedCallSites)
        if enableDebug then
          local ok, err = pcall(error, "Callsite Here", 3)
          err = err or "" -- Get rid of annoying IDE warnings
          local findIndex = err:find(": Callsite Here") or #err + 1
          local callsite = err:sub(1, findIndex - 1)
          if callsite ~= "" then
            mCollectedCallSites[callsite] = true
          end
        end

        return f(allArgs, mCollectedCallSites)
      end
    end
  end

  
  return f({}, cloneTable(extraCallSites or {}))
end
--

local function createDefaultDriver()
  local driver = {}
  driver.reduceModName = function(name) return name end
  driver.loadmod = require

  return driver
end

local function createBaseCtx(args, options)
  local myOptions = defaultTable(options or {}, {
    driver = createDefaultDriver(),
  })
  return {
    options = myOptions,
    loadedModFiles = {},
    autoloadMods = {},
    programArgs = args,
    debugRef = {{}},
  }
end

local importMod
local function createCtxUsingBase(baseCtx)
  local ctx = {
    baseCtx = baseCtx,
    functions = {},
    args = {},
    types = {},
    traits = {},
    moduleExports = {},
    using = {
      functions = {},
      types = {},
      traits = {},
    },
    currentModule = "",
    disableArgumentResolution = false
  }
  for k, v in pairs(baseCtx) do
    ctx[k] = v
  end
  for k, v in pairs(ctx.autoloadMods) do
    importMod(ctx, ctx.loadedModFiles[v])
  end
  return ctx
end

local function createCtx(args, options)
  local baseCtx = createBaseCtx(args, options)
  return createCtxUsingBase(baseCtx)
end

local function deriveScopeCtx(ctx)
  local newCtx = {
    baseCtx = ctx.baseCtx,
    functions = cloneTable(ctx.functions),
    args = cloneTable(ctx.args),
    types = cloneTable(ctx.types),
    traits = cloneTable(ctx.traits),
    moduleExports = cloneTable(ctx.moduleExports),
    using = {
      functions = cloneTable(ctx.using.functions),
      types = cloneTable(ctx.using.types),
      traits = cloneTable(ctx.using.traits),
    },
    currentModule = ctx.currentModule,
    disableArgumentResolution = ctx.disableArgumentResolution
  }
  for k, v in pairs(ctx) do
    newCtx[k] = v
  end

  return newCtx
end

local exec = funcify(function(ctx, ...)
  local funcs = { ... }
  for k, v in ipairs(funcs) do
    eval(ctx, v)
  end
end)

local function run(program, args, options)
  local ctx = createCtx(args, options)
  options = options or {}

  local builtinMods = options.builtinMods or {}
  for modName, mod in pairs(builtinMods) do
    ctx.loadedModFiles[modName] = mod
    table.insert(ctx.autoloadMods, modName)
  end

  eval(ctx, program)
end

--

local function tblSeq(tbl)
  return {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if i > #tbl then
        return SEQ_DONE_SYMBOL
      end
      return tbl[i]
    end
  }
end

local function strSeq(str)
  return {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if i > #str then
        return SEQ_DONE_SYMBOL
      end
      return str:sub(i, i)
    end
  }
end

local function iteratorSeq(it)
  local cache, i = {}, 1
  return {
    [SEQ_SYMBOL] = true,
    at = function(j)
      if j < i then
        return cache[j]
      end
      for k = i, j do
        local value = it()
        if value == SEQ_DONE_SYMBOL then
          return SEQ_DONE_SYMBOL
        end
        cache[k] = value
        i = i + 1
      end
      return cache[j]
    end
  }
end

local function seqToTable(seq, refTbl)
  if not seq[SEQ_SYMBOL] then
    return seq -- Already in normal table form
  end

  local newTbl = {}
  for k, v in pairs(refTbl or {}) do
    if type(k) ~= "number" then
      newTbl[k] = v
    end
  end

  local running, i = true, 1
  while running do
    local value = seq.at(i)
    i = i + 1
    if value == SEQ_DONE_SYMBOL then
      running = false
    else
      table.insert(newTbl, value)
    end
  end

  return newTbl
end

local function seqToOriginal(seq, ref)
  if type(ref) == "table" and ref[SEQ_SYMBOL] then
    return seq
  elseif type(ref) == "table" then
    return seqToTable(seq, ref)
  elseif type(ref) == "string" then
    return table.concat(seqToTable(seq))
  else
    error("Cannot convert sequence to type " .. type(ref))
  end
end

local function seqify(tbl)
  if type(tbl) == "string" then
    return strSeq(tbl)
  elseif not tbl[SEQ_SYMBOL] then
    return tblSeq(tbl)
  end
  return tbl
end

--

local function luaf(func)
  return funcify(function(ctx, ...)
    return func(table.unpack(evalAll(ctx, { ... })))
  end)
end

local function luaftbl(func)
  return funcify(function(ctx, ...)
    return tblSeq({ func(evalAll(ctx, ...)) })
  end)
end

local wrap = funcify(function(ctx, func)
  return func
end, 1)

local unwrap = funcify(function(ctx, func, ...)
  return eval(ctx, func(...)) -- TODO: Is this right?
end)

local only = funcify(function(ctx, optValue)
  return optValue, optValue and 1 or 0
end, 0)

local f = keyFunc(funcify(function(ctx, name, nameStr, ...)
  local args = { ... }

  local callFunc = #name == 1 and ctx.functions[name[1]] or ctx.using.functions[nameStr]
  if callFunc then
    if #args == 0 then
      return callFunc(EVAL_SYMBOL, ctx)
    else
      return callFunc(...)(EVAL_SYMBOL, ctx)
    end
  end

  error("f: No function named " .. nameStr)
end))

local function wrapUserFunction(scopeCtx, argList, func)
  return funcify(function(ctx2, ...)
    local innerArgs = { ... }
    local namedArgs = {}
    for i = 1, #argList do
      namedArgs[argList[i]] = table.remove(innerArgs, 1)
    end
    -- TODO: What should be inherited from ctx, and what should be inherited from ctx2?
    local newCtx = cloneTable(scopeCtx)
    newCtx.args = cloneTable(scopeCtx.args)
    for k, v in pairs(namedArgs) do
      newCtx.args[k] = v
    end
    return eval(newCtx, func, table.unpack(innerArgs))
  end, #argList)
end

-- TODO: Nameless defn for some reason doesn't work as intended. Use "" as name instead.
local defn = funcify(function(ctx, ...)
  local args  = { ... }
  
  local index = 1
  local name, func, argList = "", nil, { [ARGS_SYMBOL] = true, args = {} }
  if type(args[index] == "string") then
    name = args[index]
    index = index + 1
  end
  if args[index + 1] then
    argList = eval(ctx, args[index])
    index = index + 1
  end
  func = args[index]

  if not func then
    error("defn: No function provided")
  end
  if not argList[ARGS_SYMBOL] then
    error("defn: Not a valid arg list")
  end
  argList = argList.args
  
  local scopeCtx = deriveScopeCtx(ctx)
  local wrappedFunc = wrapUserFunction(scopeCtx, argList, func)
  if name ~= "" then
    ctx.functions[name] = wrappedFunc
    scopeCtx.functions[name] = wrappedFunc
  end

  return wrappedFunc, index
end, 1, true)

local a = keyFunc(funcify(function(ctx, name)
  if #name ~= 1 then
    error("a: Invalid argument")
  end
  return { [ARG_SYMBOL] = true, name = name[1] }
end))

local argsF = funcify(function(ctx, ...)
  return { [ARGS_SYMBOL] = true, args = { ... } }
end)

--

local newtype = funcify(function(ctx, name, ...)
  local args = { ... }
  local eArgs = {}
  local firstArg = #args > 0 and args[1] or nil
  if firstArg and type(firstArg) == "table" and firstArg[ARGS_SYMBOL] then
    eArgs = args[1].args
  else
    table.insert(eArgs, firstArg)
    for i = 2, #args do
      table.insert(eArgs, args[i])
    end
  end

  local ntype = {
    [CUSTOM_TYPE_SYMBOL] = true,
    name = (ctx.currentModule ~= "" and ctx.currentModule .. "." or "") .. name,
    args = eArgs,
    instances = {}
  }
  ctx.types[name] = ntype
end)

local eq2i
eq2i = function(ctx, a, b, argBinder)
  local aValue = eval(ctx, a)
  local bValue = eval(ctx, b)

  if aValue == ANY_SYMBOL or bValue == ANY_SYMBOL then
    return true
  end
  if type(aValue) == "table" and aValue[ARG_SYMBOL] and argBinder then
    argBinder[aValue.name] = bValue
    return true
  elseif type(bValue) == "table" and bValue[ARG_SYMBOL] and argBinder then
    argBinder[bValue.name] = aValue
    return true
  end

  if type(aValue) ~= type(bValue) then
    return false
  end
  if type(aValue) == "table" and aValue[INSTATIATED_TYPE_SYMBOL] then
    if aValue.type ~= bValue.type then
      return false
    end
    for i = 1, #aValue.args do
      if not eq2i(ctx, aValue.args[i], bValue.args[i], argBinder) then
        return false
      end
    end
    return true
  end

  return aValue == bValue
end

local eq2 = funcify(function(ctx, a, b)
  return eq2i(ctx, a, b)
end, 2)

local member = funcify(function(ctx, name, value)
  if type(value) == "table" and not value[INSTATIATED_TYPE_SYMBOL] then
    return value[name]
  end

  if type(value) ~= "table" then
    error("member: Not an instatiated type")
  end
  local ntype = value.type

  for i = 1, #ntype.args do
    if ntype.args[i] == name then
      return value.args[i]
    end
  end

  error("member: No member named " .. name)
end, 2)

local match = funcify(function(ctx, value, ...)
  local args = { ... }
  local firstArg = eval(ctx, value)
  local noArgCtx = cloneTable(ctx)
  noArgCtx.disableArgumentResolution = true
  for i = 1, #args, 2 do
    local compValue = args[i]
    local nextArg = args[i + 1]
    local boundArgs = {}
    if nextArg == nil then
      return eval(ctx, compValue)
    elseif eq2i(noArgCtx, wrap(firstArg), compValue, boundArgs) then
      local newCtx = cloneTable(ctx)
      newCtx.args = cloneTable(ctx.args)
      for k, v in pairs(boundArgs) do
        newCtx.args[k] = v
      end
      return eval(newCtx, nextArg)
    end
  end
end, nil, true)

local any = funcify(function(ctx)
  return ANY_SYMBOL
end, 0)

local typeConstructor = function(ctx, name, nameStr, ...)
  local ntype = (#name == 1 and ctx.types[name[1]]) or ctx.using.types[nameStr]
  if not ntype then
    error("t: No type named " .. nameStr)
  end

  local args = { ... }
  if #args ~= #ntype.args then
    error("t: Incorrect number of arguments")
  end

  local eArgs = {}
  for i = 1, #args do
    table.insert(eArgs, args[i])
  end

  return {
    [INSTATIATED_TYPE_SYMBOL] = true,
    type = ntype,
    args = eArgs
  }
end
local t = keyFunc(funcify(typeConstructor))

--

local deftrait = funcify(function(ctx, traitName, ...)
  local args = { ... }
  local fullName = (ctx.currentModule ~= "" and ctx.currentModule .. "." or "") .. traitName
  local trait = {
    name = fullName,
    methods = {},
    extends = {}
  }
  for i = 1, #args do
    local arg = args[i]
    if type(arg) == "table" and arg[METHOD_DEFINITION_SYMBOL] then
      arg.addToTraitOrInstance(trait)
    elseif type(arg) == "table" and arg[EXTENDS_SYMBOL] then
      table.insert(trait.extends, arg.name)
    else
      error("deftrait: Invalid argument")
    end
  end

  ctx.traits[traitName] = trait
end)

local defmethod = funcify(function(ctx, name, ...)
  local args = { ... }
  local methodName = eval(ctx, name)
  local argList = { [ARGS_SYMBOL] = true, args = {} }
  local func = nil
  if args[2] then
    argList = eval(ctx, args[1])
    func = args[2]
  else
    func = args[1]
  end

  if not argList[ARGS_SYMBOL] then
    error("deftrait: Not a valid arg list")
  end
  argList = argList.args
  table.insert(argList, 1, "self")

  local wrappedFunc
  if func then
    local scopeCtx = deriveScopeCtx(ctx)
    wrappedFunc = wrapUserFunction(scopeCtx, argList, func)
  end

  return {
    [METHOD_DEFINITION_SYMBOL] = true,
    addToTraitOrInstance = function(traitOrInstance, errOnNew)
      if not traitOrInstance.methods[methodName] and errOnNew then
        error("defmethod: Method " .. methodName .. " not valid for trait")
      end
      traitOrInstance.methods[methodName] = {
        argList = argList,
        wrappedFunc = wrappedFunc
      }
    end
  }, args[2] and 2 or 1
end, nil, true)

local instance = funcify(function(ctx, typeName, traitName, ...)
  local args = {...}
  local ntype = ctx.types[typeName] or ctx.using.types[typeName]
  local trait = ctx.traits[traitName] or ctx.using.traits[traitName]
  
  if not ntype then
    error("instance: No type named " .. typeName)
  end
  if not trait then
    error("instance: No trait named " .. traitName)
  end
  if ntype.instances[trait.name] then
    error("instance: Instance already exists")
  end
  for _, extendedTraitName in ipairs(trait.extends) do
    if not ntype.instances[extendedTraitName] then
      error("instance: Missing extended trait " .. extendedTraitName)
    end
  end

  local instance = {
    methods = cloneTable(trait.methods),
  }

  for i = 1, #args do
    local arg = args[i]
    if type(arg) == "table" and arg[METHOD_DEFINITION_SYMBOL] then
      arg.addToTraitOrInstance(instance, true)
    else
      error("deftrait: Invalid argument")
    end
  end

  for k, v in pairs(instance.methods) do
    if not v.wrappedFunc then
      error("instance: Missing method definition " .. k)
    end
  end

  ntype.instances[trait.name] = instance
end)

local extends = funcify(function(ctx, name)
  local trait = ctx.traits[name] or ctx.using.traits[name]
  if not trait then
    error("extends: No trait named " .. name)
  end
  return {
    [EXTENDS_SYMBOL] = true,
    name = trait.name
  }
end, 1)

local method = funcify(function(ctx, traitName, methodName, value)
  if not (type(value) == "table" and value[INSTATIATED_TYPE_SYMBOL]) then
    error("method: Not an instatiated type")
  end

  local ntrait = ctx.traits[traitName] or ctx.using.traits[traitName]
  local trait = value.type.instances[(ntrait or {}).name]
  if not trait then
    if ntrait then
      error("method: Instance does not implement trait " .. traitName)
    else
      error("method: No trait named " .. traitName)
    end
  end

  local method = trait.methods[methodName]
  if not method then
    error("method: No method named " .. methodName)
  end

  return method.wrappedFunc(value)
end, 3)

local isInstance = funcify(function(ctx, typeName, traitName)
  local ntype = ctx.types[typeName]
  local trait = ctx.traits[traitName] or ctx.using.traits[traitName]
  if not ntype then
    error("isInstance: No type named " .. typeName)
  end
  if not trait then
    error("isInstance: No trait named " .. traitName)
  end

  local instances = ctx.instances[ntype.name]
  if not instances then
    return false
  end

  local instance = instances[traitName]
  return instance and true or false
end, 2)

--

local function execMod(ctx, modName, mod)
  local newCtx = createCtxUsingBase(ctx.baseCtx)
  newCtx.currentModule = modName
  ctx.loadedModFiles[modName] = "loading"
  local oldCallSites = newCtx.debugRef[1]
  newCtx.debugRef[1] = {}
  eval(newCtx, mod)
  newCtx.debugRef[1] = oldCallSites

  return newCtx.moduleExports
end

importMod = function(ctx, allExports)
  for eModName, v in pairs(allExports) do
    for type, exports in pairs(v) do
      for expName, value in pairs(exports) do
        ctx.using[type][eModName .. "." .. expName] = value
      end
    end
  end
end

local function loadmodi(ctx, name)
  local modName = eval(ctx, name)
  local driver = ctx.options.driver
  local reducedModName = driver.reduceModName(modName)
  if ctx.loadedModFiles[reducedModName] == "loading" then
    error("loadmod: Circular dependency detected")
  end
  if not ctx.loadedModFiles[reducedModName] then
    local mod = driver.loadmod(reducedModName)
    if not mod then
      error("loadmod: Could not load module " .. modName)
    end
    
    ctx.loadedModFiles[reducedModName] = execMod(ctx, reducedModName, mod)
  end
  
  importMod(ctx, ctx.loadedModFiles[reducedModName])
end

local loadmod = funcify(function(ctx, ...)
  local args = { ... }
  for k, v in ipairs(args) do
    loadmodi(ctx, v)
  end
end)

local customMod = funcify(function(ctx, name, ...)
  local modName = eval(ctx, name)
  local modCode = exec(...)
  ctx.loadedModFiles[modName] = execMod(ctx, modName, modCode)

  importMod(ctx, ctx.loadedModFiles[modName])
end, nil, true)

local modulename = funcify(function(ctx, name)
  ctx.currentModule = name or ctx.currentModule
  return ctx.currentModule
end, 1)

local using = funcify(function(ctx, source, target)
  target = target or ""
  for k, type in ipairs({ "functions", "types", "traits" }) do
    ctx.using[type][target] = ctx.using[type][source]
    local asPrefixSource = source .. "."
    local asPrefixTarget = target == "" and "" or target .. "."
    local newUsing = {} -- Table modifications during iteration cause random issues
    for expName, value in pairs(ctx.using[type]) do
      if expName:sub(1, #asPrefixSource) == asPrefixSource then
        newUsing[asPrefixTarget .. expName:sub(#asPrefixSource + 1)] = value
      end
    end
    for expName, value in pairs(newUsing) do
      ctx.using[type][expName] = value
    end
  end
end, 1)

local export = funcify(function(ctx, type, ...)
  local args = { ... }
  ctx.moduleExports[ctx.currentModule] = ctx.moduleExports[ctx.currentModule] or {
    functions = {},
    types = {},
    traits = {},
  }
  for k, v in ipairs(args) do
    local name = v
    ctx.moduleExports[ctx.currentModule][type][name] = ctx[type][name]
  end
end)

local exportsf = export "functions"
local exportst = export "types"
local exportstr = export "traits"

--

local format
format = function(ctx, value)
  if type(value) == "table" and value[SEQ_SYMBOL] then
    local result, first, running, i = "(seq ", true, true, 1
    while running do
      local nextValue = value.at(i)
      i = i + 1
      if nextValue == SEQ_DONE_SYMBOL then
        running = false
      else
        if not first then
          result = result .. " "
        end
        result = result .. format(ctx, nextValue)
        first = false
      end
    end
    return result .. ")"
  elseif type(value) == "table" and value[INSTATIATED_TYPE_SYMBOL] then
    local result = "(t." .. value.type.name
    for k, v in ipairs(value.args) do
      result = result .. " " .. format(ctx, v)
    end
    return result .. ")"
  elseif value == ANY_SYMBOL then
    return "any"
  else
    return tostring(value)
  end
end

--

local add = funcify(function(ctx, ...)
  local args = { ... }
  local sum = 0
  for k, v in ipairs(args) do
    sum = sum + v
  end
  return sum
end, 2)

local sub = funcify(function(ctx, ...)
  local args = { ... }
  local diff = args[1]
  for i = 2, #args do
    diff = diff - args[i]
  end
  return diff
end, 2)

local mul = funcify(function(ctx, ...)
  local args = { ... }
  local product = 1
  for k, v in ipairs(args) do
    product = product * v
  end
  return product
end)

local div = funcify(function(ctx, ...)
  local args = { ... }
  local quotient = args[1]
  for i = 2, #args do
    quotient = quotient / args[i]
  end
  return quotient
end)

local mod = funcify(function(ctx, a, b)
  return a % b
end, 2)

local inc = add (1)
local dec = add (-1)
local neg = mul (-1)

--

local eq = funcify(function(ctx, ...)
  local args = { ... }
  local value = args[1]
  for i = 2, #args do
    local v = args[i]
    if value ~= v then
      return false
    end
  end
  return true
end)

local lt = funcify(function(ctx, a, b)
  return a < b
end, 2)

local gt = funcify(function(ctx, a, b)
  return a > b
end)

local lte = funcify(function(ctx, a, b)
  return a <= b
end, 2)

local gte = funcify(function(ctx, a, b)
  return a >= b
end, 2)

local neq = funcify(function(ctx, a, b)
  return a ~= b
end, 2)

--

local seq = funcify(function(ctx, ...)
  return tblSeq({ ... })
end)

local toseq = funcify(function(ctx, value)
  return seqify(value)
end, 1)

local applySeq = function(args)
  local applyValue = {
    [APPLY_SEQ_SYMBOL] = true,
    seq = args
  }
  return setmetatable(applyValue, {
    __call = function(_, ...)
      return applyValue
    end
  })
end

--

local and_ = funcify(function(ctx, ...)
  local args = { ... }
  local lastVal
  for k, v in ipairs(args) do
    lastVal = v
    if not lastVal then
      return false
    end
  end
  return lastVal
end)

local or_ = funcify(function(ctx, ...)
  local args = { ... }
  for k, v in ipairs(args) do
    local val = v
    if val then
      return val
    end
  end
  return false
end)

local not_ = funcify(function(ctx, a)
  return not a
end, 1)

local xor = funcify(function(ctx, a, b)
  return a ~= b
end, 2)

--

local function evalOp(code, ...)
  local argArray = ""
  for k, v in ipairs({ ... }) do
    argArray = argArray .. ", " .. v
  end
  argArray = argArray:sub(3)
  local func = load("return function(ctx, " .. argArray .. ") return " .. code .. " end")
  if not func then return end -- Hopefully, the program isn't using this
  return func()
end

local shl, shr, ashr, band, bor, bxor, bnot
if bit32 then
  shl = luaf(bit32.lshift)
  shr = luaf(bit32.rshift)
  ashr = luaf(bit32.arshift)
  band = luaf(bit32.band)
  bor = luaf(bit32.bor)
  bxor = luaf(bit32.bxor)
  bnot = luaf(bit32.bnot)
else
  shl = evalOp("a << b", "a", "b")
  shr = evalOp("a >> b", "a", "b")
  ashr = evalOp("a >> b", "a", "b")
  band = evalOp("a & b", "a", "b")
  bor = evalOp("a | b", "a", "b")
  bxor = evalOp("a ~ b", "a", "b")
  bnot = evalOp("~a", "a")
end


--

local cond = funcify(function(ctx, ...)
  local args = { ... }
  for i = 1, #args, 2 do
    local value = args[i]
    local nextArg = args[i + 1]
    if nextArg == nil then
      return eval(ctx, value)
    elseif eval(ctx, value) then
      return eval(ctx, nextArg)
    end
  end
end, nil, true)

local with = funcify(function(ctx, ...)
  local args = {...}

  local newCtx = cloneTable(ctx)
  newCtx.args = cloneTable(ctx.args)
  for i = 1, #args, 2 do
    newCtx.args[args[i]] = eval(newCtx, args[i + 1])
  end

  local func = args[#args]
  return eval(newCtx, func)
end, nil, true)

local prints = funcify(function(ctx, ...)
  local args = { ... }
  local str = ""
  for k, v in ipairs(args) do
    str = str .. format(ctx, v) .. " "
  end
  print(str:sub(1, #str - 1))
end)

--

local map = funcify(function(ctx, func, initialVal)
  local cache = {}
  local myArgs = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if not cache[i] then
        local value = myArgs.at(i)
        if value == SEQ_DONE_SYMBOL then
          return SEQ_DONE_SYMBOL
        end
        cache[i] = eval(ctx, func, value)
      end
      return cache[i]
    end
  }

  return seqToOriginal(newSeq, initialVal)
end, 2)

local filter = funcify(function(ctx, func, initialVal)
  local cache, index, innerIndex = {}, 1, 1
  local myArgs = seqify(initialVal)

  local function next()
    while true do
      local nextVal = myArgs.at(innerIndex)
      innerIndex = innerIndex + 1
      if nextVal == SEQ_DONE_SYMBOL then
        return SEQ_DONE_SYMBOL
      end
      if eval(ctx, func, nextVal) then
        cache[index] = nextVal
        index = index + 1
        return nextVal
      end
    end
  end

  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if cache[i] then
        return cache[i]
      end

      for j = index, i do
        local nextVal = next()
        if nextVal == SEQ_DONE_SYMBOL then
          return SEQ_DONE_SYMBOL
        end
      end

      return cache[i]
    end
  }

  return seqToOriginal(newSeq, initialVal)
end, 2)

local foldl = funcify(function(ctx, func, acc, args)
  local result = acc
  for k, v in ipairs(seqToTable(args)) do
    result = eval(ctx, func, result, v)
  end
  return result
end, 3)

local foldr = funcify(function(ctx, func, acc, args)
  local result = acc
  local tbl = seqToTable(args)
  for i = #tbl, 1, -1 do
    result = eval(ctx, func, tbl[i], result)
  end
  return result
end, 3)

local iterator = funcify(function(ctx, start, stepSize, numSteps)
  local index = start or 1
  local i = 0
  return iteratorSeq(function()
    i = i + 1
    if numSteps and i > numSteps then
      return SEQ_DONE_SYMBOL
    end
    local value = index
    index = index + (stepSize or 1)
    return value
  end), #{start, stepSize, numSteps}
end)

local range = funcify(function(ctx, start, stop, stepSize)
  local index = start
  local stopIndex = stop
  local size = stepSize or 1
  local i = 0
  return iteratorSeq(function()
    i = i + 1
    if (size > 0 and index > stopIndex) or (size < 0 and index < stopIndex)then
      return SEQ_DONE_SYMBOL
    end
    local value = index
    index = index + size
    return value
  end), #{start, stop, stepSize}
end, 2)

local cycle = funcify(function(ctx, ...)
  local args = {...}
  local eArgs = {}
  for k, v in ipairs(args) do
    table.insert(eArgs, v)
  end

  local i = 0
  return iteratorSeq(function()
    i = i + 1
    if i > #eArgs then
      i = 1
    end
    return eArgs[i]
  end)
end)

local take = funcify(function(ctx, n, initialVal)
  local myArgs = seqify(initialVal)
  local i = 0
  local newSeq = iteratorSeq(function()
    i = i + 1
    if i > n then
      return SEQ_DONE_SYMBOL
    end
    return myArgs.at(i)
  end)

  return seqToOriginal(newSeq, initialVal)
end, 2)

local skip = funcify(function(ctx, n, initialVal)
  local mySeq = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      return mySeq.at(i + n)
    end
  }

  return seqToOriginal(newSeq, initialVal)
end, 2)

local head = funcify(function(ctx, args)
  return seqify(args).at(1)
end, 1)

local tail = funcify(function(ctx, initialVal)
  local mySeq = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      return mySeq.at(i + 1)
    end
  }
  return seqToOriginal(newSeq, initialVal)
end, 1)

local subseq = funcify(function(ctx, start, num, initialVal)
  local mySeq = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if i > num then
        return SEQ_DONE_SYMBOL
      end
      return mySeq.at(i + start - 1)
    end
  }
  return seqToOriginal(newSeq, initialVal)
end, 3)

local splice = funcify(function(ctx, start, num, initialVal)
  local mySeq = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if i < start then
        return mySeq.at(i)
      end
      return mySeq.at(i + num)
    end
  }
  return seqToOriginal(newSeq, initialVal)
end, 3)

local concat = funcify(function(ctx, ...)
  local args = { ... }
  local subseqs = {}
  for k, v in ipairs(args) do
    table.insert(subseqs, seqify(v))
  end
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      local offset = 0
      for k, v in ipairs(subseqs) do
        local value = v.at(i - offset)
        if value == SEQ_DONE_SYMBOL then
          offset = offset + #seqToTable(v)
        else
          return value
        end
      end
      return SEQ_DONE_SYMBOL
    end
  }

  if type(args[1]) == "table" and not args[1][SEQ_SYMBOL] then
    local result = seqToTable(newSeq, args[1])
    for i = 2, #args do
      if type(args[i]) == "table" and not args[1][SEQ_SYMBOL] then
        for k, v in pairs(args[i]) do
          if type(k) ~= "number" then
            result[k] = v
          end
        end
      end
    end
  end

  return seqToOriginal(newSeq, args[1])
end)

local count = funcify(function(ctx, seq)
  local mySeq = seqify(seq)
  local i = 0
  while mySeq.at(i + 1) ~= SEQ_DONE_SYMBOL do
    i = i + 1
  end

  return i
end, 1)

--

local upper = luaf (string.upper)
local lower = luaf (string.lower)

local str = funcify(function(ctx, ...)
  local args = { ... }
  local str = ""
  for k, v in ipairs(args) do
    if type(v) == "string" then
      str = str .. v
    elseif type(v) == "table" and v[SEQ_SYMBOL] then
      str = str .. table.concat(seqToTable(v))
    else
      str = str .. format(ctx, v)
    end
  end

  return str
end, 1)

--

local num = funcify(function(ctx, value)
  local asNum = tonumber(value)
  if not asNum then
    error("num: Cannot convert to number")
  end
  return asNum
end, 1)

--

local tbl = funcify(function(ctx, value)
  if value == nil then
    return {}
  end
  if type(value) == "table" and value[SEQ_SYMBOL] then
    return seqToTable(value)
  end
  if type(value) == "table" then
    return value
  end
  error("tbl: Don't know how to convert to table")
end, 1)

local withKeys = funcify(function(ctx, value, ...)
  local args = { ... }
  local mtbl = cloneTable(value, true)
  for i = 1, #args, 2 do
    mtbl[args[i]] = args[i + 1]
  end

  return mtbl
end)

local newTbl = withKeys {}

local keys = funcify(function(ctx, value)
  local eValue = eval(ctx, value)
  local keyList = {}
  for k, v in pairs(eValue) do
    table.insert(keyList, k)
  end
  return tblSeq(keyList)
end, 1)

local values = funcify(function(ctx, value)
  local valueList = {}
  for k, v in pairs(value) do
    table.insert(valueList, v)
  end
  return tblSeq(valueList)
end, 1)

--

local getArgs = funcify(function(ctx)
  return seqify(ctx.programArgs)
end, 0)

--

local builtinLib = customMod "builtin"
  (deftrait "alternative"
    (defmethod "|" (argsF "other")))
  (deftrait "monad"
    (extends "alternative")
    (defmethod ">>=" (argsF "transform"))
    (defmethod ">>" (argsF "next")))
  (newtype "just" "a")
  (newtype "empty")
  (newtype "left" "a")
  (newtype "right" "b")
  (newtype "failure" "reason")
  (defn ">>=" (method "monad" ">>="))
  (defn ">>" (method "monad" ">>"))
  (exportst "just" "empty" "left" "right" "failure")
  (exportstr "alternative" "monad")
  (exportsf ">>=" ">>")

--

local environment = {
  exec = exec,
  run = run,
  wrap = wrap,
  unwrap = unwrap,
  only = only,
  f = f,
  defn = defn,
  a = a,
  args = argsF,
  newtype = newtype,
  eq2 = eq2,
  member = member,
  match = match,
  any = any,
  _ = any, -- Alias
  t = t,
  defmethod = defmethod,
  deftrait = deftrait,
  instance = instance,
  extends = extends,
  method = method,
  isInstance = isInstance,
  loadmod = loadmod,
  modulename = modulename,
  using = using,
  exportsf = exportsf,
  exportst = exportst,
  exportstr = exportstr,
  luaf = luaf,
  luaftbl = luaftbl,
  add = add,
  sub = sub,
  mul = mul,
  div = div,
  mod = mod,
  inc = inc,
  dec = dec,
  neg = neg,
  eq = eq,
  lt = lt,
  gt = gt,
  lte = lte,
  gte = gte,
  neq = neq,
  seq = seq,
  toseq = toseq,
  applySeq = applySeq,
  and_ = and_,
  or_ = or_,
  not_ = not_,
  xor = xor,
  shl = shl,
  shr = shr,
  ashr = ashr,
  band = band,
  bor = bor,
  bxor = bxor,
  bnot = bnot,
  cond = cond,
  with = with,
  prints = prints,
  map = map,
  filter = filter,
  foldl = foldl,
  foldr = foldr,
  iterator = iterator,
  range = range,
  cycle = cycle,
  take = take,
  skip = skip,
  head = head,
  tail = tail,
  subseq = subseq,
  splice = splice,
  concat = concat,
  count = count,
  upper = upper,
  lower = lower,
  str = str,
  num = num,
  tbl = tbl,
  withKeys = withKeys,
  newTbl = newTbl,
  keys = keys,
  values = values,
  getArgs = getArgs,
}

local rtn = {}
local function install(env)
  for k, v in pairs(environment) do
    env[k] = v
  end
end
install(rtn)

local function printCallSite(collectedCallSites)
  print("---\nThe Funclu library reported an error. If you believe this is a Funclu bug, " ..
    "please report it at " .. ISSUES_URL .. "\n---")

  local hasCallSite = not not (pairs(collectedCallSites)(collectedCallSites))
  if not hasCallSite then
    print("No call site information available")
    return
  end
  print("Potentially Relevant Call Sites:")
  for callSite in pairs(collectedCallSites) do
    print(callSite)
  end
end

local function installAndRun(env, args, options)
  install(env)
  local ctx = createCtx(args, options)
  eval(ctx, builtinLib)
  table.insert(ctx.autoloadMods, "builtin")
  local func
  func = function(line)
    if enableDebug then
      ctx.debugRef[1] = {}
      local ok, err = pcall(eval, ctx, line)
      if not ok then
        printCallSite(ctx.debugRef[1])
        error(err, -1)
      end
    else
      eval(ctx, line)
    end
    return func
  end

  return func
end
rtn.install = installAndRun

rtn.modules = function(env)
  install(env)
  return exec
end

rtn.enableDebug = function(b)
  enableDebug = b ~= false
  return rtn
end

rtn.custom = {
  funcify = funcify,
  eval = eval,
  customMod = customMod,
  execMod = execMod,
  importMod = importMod,
}

return rtn