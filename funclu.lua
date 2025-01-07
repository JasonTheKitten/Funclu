local ANY_SYMBOL = {}
local APPLY_SEQ_SYMBOL = {}
local ARGS_SYMBOL = {}
local ARG_SYMBOL = {}
local BIND_SYMBOL = {}
local BIND_VALUE_SYMBOL = {}
local CUSTOM_TYPE_SYMBOL = {}
local EVAL_SYMBOL = {}
local EXTENDS_SYMBOL = {}
local INSTANTIATED_TYPE_SYMBOL = {}
local METHOD_DEFINITION_SYMBOL = {}
local NATIVE_TYPE_SYMBOL = {}
local SCOPED_ARG_SYMBOL = {}
local SEQ_SYMBOL = {}
local SEQ_DONE_SYMBOL = {}
local UNRESOLVED_KEY_SYMBOL = {}
local VOID_SYMBOL = {}

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

local function removeEvalWrapper(res)
  while (type(res) == "table") and res[SCOPED_ARG_SYMBOL] do
    res = res.value
  end
  return res
end

local eval
eval = function(ctx, func, ...)
  local res = func
  while (type(res) == "table") and res[SCOPED_ARG_SYMBOL] do
    ctx = res.ctx
    res = res.value
  end
  if type(res) == "table" and res[UNRESOLVED_KEY_SYMBOL] then
    res = res()
  end
  if type(res) == "function" then
    local args = { ... }
    if #args > 0 then
      res = res(...)
    end
    res = res(EVAL_SYMBOL, ctx)
  end
  if (type(res) == "table") and res[ARG_SYMBOL] and not ctx.disableArgumentResolution then
    return eval(ctx, ctx.args[res.name])
  end
  return res
end

local function evalMany(ctx, args, num)
  local eArgs = {}
  for i = 1, (num or #args) do
    eArgs[i] = eval(ctx, args[i])
  end
  return eArgs
end

local funcify
local function evalLater(ctx, innerVal)
  local cache
  return funcify(function()
    if not cache then
      cache = eval(ctx, innerVal)
    end
    return cache
  end, 0)
end

local function scopeArg(ctx, arg)
  if (type(arg) ~= "table") and (type(arg) ~= "function") then
    return arg
  end
  if (type(arg) == "table") and (arg[SCOPED_ARG_SYMBOL] or arg[ARG_SYMBOL]) then
    return arg
  end
  return {
    [SCOPED_ARG_SYMBOL] = true,
    ctx = ctx,
    value = arg
  }
end

local function rescopeArg(ctx, arg)
  return scopeArg(ctx, removeEvalWrapper(arg))
end

local function applyWithinScope(func, args)
  if type(func) == "function" then
    return func(table.unpack(args))
  end
  if type(removeEvalWrapper(func)) == "function" then
    return scopeArg(func.ctx, removeEvalWrapper(func)(args))
  end
  return func
end

local function evalIfApplied(ctx, func, args)
  if #args == 0 then
    return func
  end
  return eval(ctx, applyWithinScope(func, args))
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

local function evalFunction(ctx, func, eArgs, disableAutoEval, argsData)
  local completed =
    (type(argsData) == "number" and #eArgs >= argsData)
    or (type(argsData) ~= "number")

  if completed then
    if disableAutoEval then
      -- Only resolve arguments if we're not auto-evaluating
      for i = 1, #eArgs do
        if type(eArgs[i]) == "table" and eArgs[i][ARG_SYMBOL] then
          eArgs[i] = eval(ctx, eArgs[i])
        end
      end
    else
      if type(argsData) == "number" then
        for i = 1, argsData do
          eArgs[i] = eval(ctx, eArgs[i])
        end
      else
        error("Variadic args with auto eval not supported")
      end
    end
    ctx.options.driver.preventTimeout()
    local result, actual = func(ctx, table.unpack(eArgs))
    actual = actual or (type(argsData) == "number" and argsData) or #eArgs
    if type(removeEvalWrapper(result)) == "function" then
      local remainingArgs = { select(actual + 1, table.unpack(eArgs)) }
      -- TODO: Check if below logic is right
      return evalIfApplied(ctx, result, remainingArgs)
    end
    return result
  end
  
  local remaining = argsData - #eArgs
  return funcify(function(ctx, ...)
    local margs = { ... }
    local allArgs = {}
    for k, v in ipairs(eArgs) do
      table.insert(allArgs, v)
    end
    for k, v in ipairs(margs) do
      table.insert(allArgs, scopeArg(ctx, v))
    end
    return evalFunction(ctx, func, allArgs, remaining)
  end, remaining, disableAutoEval, ctx.debugRef[1])
end

funcify = function(func, argsData, disableAutoEval, extraCallSites)
  if not disableAutoEval and (type(argsData) ~= "number") then
    error("Variadic args with auto eval not supported")
  end

  local f
  f = function(args, collectedCallSites)
    return function(...)
      local margs = { ... }
      if margs[1] == EVAL_SYMBOL then
        local ctx = margs[2]
        local eArgs = applyArgs(ctx, args)
        for i = 1, #eArgs do
          eArgs[i] = scopeArg(ctx, eArgs[i])
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

local function createBaseDriver()
  local driver = {}
  driver.reduceModName = function(name) return name end
  driver.loadmod = require
  driver.preventTimeout = function() end

  return driver
end

-- Drivers allow for ComputerCraft/OpenComputers integration
local function createCraftOSDriver()
  local driver = createBaseDriver()

  local oldTime = os.clock()
  driver.preventTimeout = function()
    local newTime = os.clock()
    if newTime > (oldTime + 2.5) then
      ---@diagnostic disable-next-line: undefined-field
      os.queueEvent("")
      coroutine.yield("")
      oldTime = newTime
    end
  end

  return driver
end

local function createOpenOSDriver()
  local driver = createBaseDriver()

  local oldTime = os.clock() * 100
  driver.preventTimeout = function()
    local newTime = os.clock() * 100
    if newTime > (oldTime + 2.5) then
      ---@diagnostic disable-next-line: undefined-global
      computer.pushSignal("")
      coroutine.yield("")
      oldTime = newTime
    end
  end

  return driver
end

local function createPucDriver()
  return createBaseDriver()
end

local function createDefaultDriver()
  return
    ---@diagnostic disable-next-line: undefined-field
    (os.queueEvent and createCraftOSDriver())
    ---@diagnostic disable-next-line: undefined-global
    or (computer and computer.pushSignal and createOpenOSDriver())
    or createPucDriver()
end

local function createBaseCtx(args, options)
  local myOptions = defaultTable(options or {}, {
    driver = createDefaultDriver(),
  })
  return {
    options = myOptions,
    loadedModFiles = {},
    providerNames = {},
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
    currentBaseModule = "",
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

-- TODO: Fine tune what is derived
local function deriveScopeCtx(ctx)
  local newCtx = {
      baseCtx = ctx.baseCtx,
      types = ctx.types,
      traits = ctx.traits
  }
  for k, v in pairs(ctx) do
    newCtx[k] = newCtx[k] or ((type(v) == "table") and cloneTable(v)) or v
  end
  newCtx.using.functions = cloneTable(ctx.using.functions)
  for k, v in pairs(ctx.baseCtx) do
    newCtx[k] = v
  end

  return newCtx
end

local exec = funcify(function(ctx, ...)
  local funcs = { ... }
  for k, v in ipairs(funcs) do
    eval(ctx, v)
  end
end, nil, true)

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

local function tblSeq(tbl, lazyCtx)
  local cache = lazyCtx and {} or nil
  return {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if i > #tbl then
        return SEQ_DONE_SYMBOL
      end
      if cache and cache[i] then
        return cache[i]
      end
      if cache then
        cache[i] = eval(lazyCtx, tbl[i])
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
  elseif type(tbl) ~= "table" then
    error("Cannot convert type " .. type(tbl) .. " to sequence")
  elseif not tbl[SEQ_SYMBOL] then
    return tblSeq(tbl)
  end
  return tbl
end

--

local void = setmetatable(VOID_SYMBOL, {
  __call = function()
    return VOID_SYMBOL
  end
})

local t
local function createErr(ctx, err)
  local asOrderedTbl = {}
  for k, v in pairs(ctx.debugRef[1]) do
    table.insert(asOrderedTbl, k)
  end
  return t.err(err, tblSeq(asOrderedTbl))
end

local ioEffect
local ioType = {
  [CUSTOM_TYPE_SYMBOL] = true,
  [NATIVE_TYPE_SYMBOL] = true,
  name = "io",
  args = {},
  instances = {
    ["builtin.alternative"] = {
      methods = {
        ["|"] = {
          argList = { "self", "other" },
          wrappedFunc = funcify(function(ctx, self, other)
            return eval(ctx, self)
          end, 2, true)
        }
      }
    },
    ["builtin.monad"] = {
      methods = {
        [">>="] = {
          argList = { "self", "transform" },
          wrappedFunc = funcify(function(ctx, self, transform)
            return ioEffect(function()
              local ok, result = pcall(eval(ctx, self).effect, ctx)
              if not ok then
                return eval(ctx, createErr(ctx, result))
              end
              return eval(ctx, transform, result)
            end)
          end, 2, true)
        }
      }
    }
  }
}

ioEffect = function(effect)
  local errorStr = "Cannot access members of IO effect"
  return {
    [INSTANTIATED_TYPE_SYMBOL] = true,
    type = ioType,
    argCount = function() error(errorStr) end,
    argAt = function() error(errorStr) end,
    effect = effect
  }
end

local function ioFuncify(func, argData)
  return funcify(function(ctx, ...)
    local args = { ... }
    return ioEffect(function()
      return func(ctx, table.unpack(evalMany(ctx, args))) or void
    end)
  end, argData, true)
end

local function isIoEffect(value)
  return
    type(value) == "table"
    and value[INSTANTIATED_TYPE_SYMBOL]
    and (value.type == ioType)
end

--

local function luaf(args)
  return function(func)
    return funcify(function(ctx, ...)
      return func(... )
    end, args)
  end
end

local function luaftbl(args)
  return function(func)
    return funcify(function(ctx, ...)
      return tblSeq({ func(...) })
    end, args)
  end
end

local wrap = funcify(function(ctx, func)
  return func
end, 1, true)

local unwrap = funcify(function(ctx, func, ...)
  return eval(ctx, func, table.unpack(evalMany(ctx, { ... }))) -- TODO: Is this right?
end, nil, true)

local only = funcify(function(ctx, optValue, ...)
  return evalIfApplied(ctx, eval(ctx, optValue), { ... })
end, nil, true)

local undefined = funcify(function(ctx)
  error("undefined: Attempt to evaluate undefined")
end, 0)

local errs = funcify(function(ctx, arg)
  error("errs: " .. tostring(eval(ctx, arg)), -1)
end, 1)

local f = keyFunc(funcify(function(ctx, name, nameStr, ...)
  local args = { ... }

  name = eval(ctx, name)
  nameStr = eval(ctx, nameStr)
  local callFunc = #name == 1 and ctx.functions[name[1]] or ctx.using.functions[nameStr]
  if callFunc then
    return eval(ctx, callFunc, table.unpack(args))
  end

  error("f: No function named " .. nameStr)
end, nil, true))

local function wrapUserFunction(scopeCtx, argList, func)
  return funcify(function(ctx2, ...)
    local innerArgs = { ... }
    local namedArgs = {}
    for i = 1, #argList do
      namedArgs[argList[i]] = evalLater(ctx2, table.remove(innerArgs, 1))
    end
    -- TODO: What should be inherited from ctx, and what should be inherited from ctx2?
    local newCtx = cloneTable(scopeCtx or ctx2)
    newCtx.args = cloneTable(newCtx.args)
    for k, v in pairs(namedArgs) do
      newCtx.args[k] = v
    end
    local innerFunc = removeEvalWrapper(func)
    local result = eval(newCtx, innerFunc)
    
    return evalIfApplied(newCtx, result, innerArgs)
  end, #argList, true)
end

local defn = funcify(function(ctx, ...)
  local args  = { ... }
  local index = 1
  local name, func, argList = "", nil, { [ARGS_SYMBOL] = true, args = {} }
  name = eval(ctx, args[index])
  index = index + 1
  
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
end, 2, true)

local a = keyFunc(funcify(function(ctx, name)
  if #name ~= 1 then
    error("a: Invalid argument")
  end
  return { [ARG_SYMBOL] = true, name = name[1] }
end, 1))

local argsF = funcify(function(ctx, ...)
  return { [ARGS_SYMBOL] = true, args = evalMany(ctx, { ... }) }
end, nil, true)

--

local newtype = funcify(function(ctx, name, ...)
  local args = evalMany(ctx, { ... })
  local eArgs = {}
  local eName = eval(ctx, name)
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
    name = (ctx.currentModule ~= "" and ctx.currentModule .. "." or "") .. eName,
    args = eArgs,
    instances = {}
  }
  ctx.types[eName] = ntype
end, nil, true)

local eq2i
eq2i = function(ctx, a, b, argBinder)
  -- TODO: Is the eval wrapper important?
  local aValue = removeEvalWrapper(eval(ctx, a))
  local bValue = removeEvalWrapper(eval(ctx, b))
  

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
  if type(aValue) == "table" and aValue[INSTANTIATED_TYPE_SYMBOL] then
    if aValue.type ~= bValue.type then
      return false
    end
    for i = 1, aValue.argCount() do
      if not eq2i(ctx, aValue.argAt(i), bValue.argAt(i), argBinder) then
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
  if type(value) == "table" and not value[INSTANTIATED_TYPE_SYMBOL] then
    return value[name]
  end

  if type(value) ~= "table" then
    error("member: Not an instatiated type")
  end
  local ntype = value.type

  if ntype[NATIVE_TYPE_SYMBOL] then
    error("member: Cannot access members of native types")
  end

  for i = 1, #ntype.args do
    if ntype.args[i] == name then
      return value.argAt(i)
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
    elseif eq2i(noArgCtx, wrap(firstArg), rescopeArg(noArgCtx, compValue), boundArgs) then
      local newCtx = cloneTable(ctx)
      newCtx.args = cloneTable(ctx.args)
      for k, v in pairs(boundArgs) do
        newCtx.args[k] = evalLater(ctx, v)
      end
      return eval(newCtx, removeEvalWrapper(nextArg))
    end
  end
end, nil, true)

local any = funcify(function(ctx)
  return ANY_SYMBOL
end, 0)

local typeConstructor = function(ctx, name, nameStr, ...)
  name = eval(ctx, name)
  nameStr = eval(ctx, nameStr)
  local ntype = (#name == 1 and ctx.types[name[1]]) or ctx.using.types[nameStr]
  if not ntype then
    error("t: No type named " .. nameStr)
  end

  local args = evalMany(ctx, { ... })
  if #args ~= #ntype.args then
    error("t: Incorrect number of arguments")
  end

  local cache = {}
  return {
    [INSTANTIATED_TYPE_SYMBOL] = true,
    type = ntype,
    argCount = function()
      return #ntype.args
    end,
    argAt = function(i)
      if cache[i] then
        return cache[i]
      end
      cache[i] = eval(ctx, args[i])
      return cache[i]
    end
  }
end
t = keyFunc(funcify(typeConstructor, nil, true))

--

local deftrait = funcify(function(ctx, traitName, ...)
  local eTraitName = eval(ctx, traitName)
  local args = evalMany(ctx, { ... })
  local fullName = (ctx.currentModule ~= "" and ctx.currentModule .. "." or "") .. eTraitName
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

  ctx.traits[eTraitName] = trait
end, nil, true)

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
  typeName = eval(ctx, typeName)
  traitName = eval(ctx, traitName)
  local args = evalMany(ctx, { ... })
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
end, nil, true)

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

local method = funcify(function(ctx, traitName, methodName)
  local ntrait = ctx.traits[traitName] or ctx.using.traits[traitName]
  if not ntrait then
    error("method: No trait named " .. traitName)
  end

  local methodDef = ntrait.methods[methodName]
  if not methodDef then
    error("method: No method named " .. methodName)
  end

  return funcify(function(ctx, value, ...)
    value = eval(ctx, value)
    if not (type(value) == "table" and value[INSTANTIATED_TYPE_SYMBOL]) then
      error("method: Not an instatiated type")
    end
  
    local trait = value.type.instances[ntrait.name]
    if not trait then
      error("method: Instance does not implement trait " .. traitName)
    end

    local method = trait.methods[methodName] or methodDef
    if not method or not method.wrappedFunc then
      error("method: Instance does not implement method " .. methodName)
    end

    return method.wrappedFunc(value, ...)
  end, #methodDef.argList, true)
end, 2)

local isInstance = funcify(function(ctx, typeName, traitName)
  local ntype = (type(typeName) == "table" and typeName.type) or ctx.types[typeName]
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
  newCtx.currentBaseModule = modName
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

local function loadmodi(ctx, name, nameOverride)
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

  return reducedModName
end

local loadmod = funcify(function(ctx, ...)
  local args = evalMany(ctx, { ... })
  for k, v in ipairs(args) do
    loadmodi(ctx, v)
  end
end, nil, true)

local loadprovider = funcify(function(ctx, providerName, defaultModule)
  providerName = eval(ctx, providerName)
  defaultModule = eval(ctx, defaultModule)
  if not ctx.providerNames[providerName] then
    if not defaultModule then
      error("loadprovider: Provider not in cache and no default module provided")
    end
    ctx.providerNames[providerName] = loadmodi(ctx, defaultModule)
  end

  importMod(ctx, ctx.loadedModFiles[ctx.providerNames[providerName]])
  defaultModule = defaultModule or ctx.providerNames[providerName]
  for k, v in pairs(ctx.using) do
    for k2, v2 in pairs(v) do
      if k2:sub(1, #defaultModule + 1) == (defaultModule .. ".") then
        local suffix = k2:sub(#defaultModule + 2)
        local newName = providerName .. "." .. suffix
        v[newName] = v2
      end
    end
    v[providerName] = v[ctx.providerNames[providerName]]
  end
end, nil, true)

local customMod = funcify(function(ctx, name, ...)
  local modName = eval(ctx, name)
  local args = { ... }
  -- Strip context info
  for i = 1, #args do
    if (type(args[i]) == "table") and args[i][SCOPED_ARG_SYMBOL] then
      args[i] = args[i].value
    end
  end

  local modCode = exec(table.unpack(args))
  ctx.loadedModFiles[modName] = execMod(ctx, modName, modCode)

  importMod(ctx, ctx.loadedModFiles[modName])
end, nil, true)

local modulename = funcify(function(ctx, name)
  ctx.currentBaseModule = name
  ctx.currentModule = name
  return ctx.currentModule
end, 1)

local submodule = funcify(function(ctx, name)
  ctx.currentModule =
    (name == "" and ctx.currentBaseModule)
    or (ctx.currentBaseModule .. "." .. name)
  return ctx.currentModule
end, 1)

local function findFuncTarget(name)
  for i = #name, 1, -1 do
    if name:sub(i, i) == "." then
      local lname = name:sub(i + 1)
      return lname ~= "" and lname or nil
    end
  end
end

local using = funcify(function(ctx, source, target)
  source = eval(ctx, source)
  target = eval(ctx, target)
  for k, etype in ipairs({ "functions", "types", "traits" }) do
    local funcTarget = target or findFuncTarget(source)
    if funcTarget and (funcTarget ~= "") then
      ctx.using[etype][funcTarget] = ctx.using[etype][source]
    end
    local asPrefixSource = source .. "."
    local asPrefixTarget = ((target == "") or not target) and "" or target .. "."
    local newUsing = {} -- Table modifications during iteration cause random issues
    for expName, value in pairs(ctx.using[etype]) do
      if expName:sub(1, #asPrefixSource) == asPrefixSource then
        newUsing[asPrefixTarget .. expName:sub(#asPrefixSource + 1)] = value
      end
    end
    for expName, value in pairs(newUsing) do
      ctx.using[etype][expName] = value
    end
  end
end, nil, true)

local export = funcify(function(ctx, type, ...)
  local args = evalMany(ctx, { ... })
  type = eval(ctx, type)
  ctx.moduleExports[ctx.currentModule] = ctx.moduleExports[ctx.currentModule] or {
    functions = {},
    types = {},
    traits = {},
  }
  for k, v in ipairs(args) do
    ctx.moduleExports[ctx.currentModule][type][v] = ctx[type][v]
  end
end, nil, true)

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
  elseif type(value) == "table" and value[INSTANTIATED_TYPE_SYMBOL] and value.type[NATIVE_TYPE_SYMBOL] then
    return "NativeType(" .. value.type.name .. ")"
  elseif type(value) == "table" and value[INSTANTIATED_TYPE_SYMBOL] then
    local result = "(t." .. value.type.name
    for i = 1, value.argCount() do
      local v = value.argAt(i)
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

local add = funcify(function(ctx, a, b)
  return a + b
end, 2)

local sub = funcify(function(ctx, a, b)
  return a - b
end, 2)

local mul = funcify(function(ctx, a, b)
  return a * b
end, 2)

local div = funcify(function(ctx, a, b)
  return a / b
end, 2)

local mod = funcify(function(ctx, a, b)
  return a % b
end, 2)

local inc = add (1)
local dec = add (-1)
local neg = mul (-1)

--

local eq = funcify(function(ctx, a, b)
  return a == b
end, 2)

local lt = funcify(function(ctx, a, b)
  return a < b
end, 2)

local gt = funcify(function(ctx, a, b)
  return a > b
end, 2)

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
  return tblSeq({ ... }, ctx)
end, nil, true)

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

local and_ = funcify(function(ctx, a, b)
  return a and b
end, 2)

local or_ = funcify(function(ctx, a, b)
  return a or b
end, 2)

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
  return funcify(func(), #{ ... })
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
  for i = 1, #args - 1, 2 do
    newCtx.args[eval(newCtx, args[i])] = evalLater(newCtx, rescopeArg(newCtx, args[i + 1]))
    newCtx = cloneTable(newCtx)
    newCtx.args = cloneTable(newCtx.args)
  end

  local func = removeEvalWrapper(args[#args])
  return eval(newCtx, func)
end, nil, true)

--

local map = funcify(function(ctx, func, initialVal)
  initialVal = eval(ctx, initialVal)
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
end, 2, true)
-- TODO: Debug why initialVal is still a scopedArg if the ", true" is removed

local filter = funcify(function(ctx, func, initialVal)
  initialVal = eval(ctx, initialVal)
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
end, 2, true)

local foldl = funcify(function(ctx, func, acc, args)
  local result = eval(ctx, acc)
  args = eval(ctx, args)
  for k, v in ipairs(seqToTable(args)) do
    result = eval(ctx, func, result, v)
  end
  return result
end, 3, true)

local foldr = funcify(function(ctx, func, acc, args)
  local result = eval(ctx, acc)
  local tbl = seqToTable(eval(ctx, args))
  for i = #tbl, 1, -1 do
    result = eval(ctx, func, tbl[i], result)
  end
  return result
end, 3, true)

local iterator = funcify(function(ctx, start, stepSize, numSteps)
  start = eval(ctx, start)
  stepSize = eval(ctx, stepSize)
  numSteps = eval(ctx, numSteps)
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
end, nil, true)

local range = funcify(function(ctx, start, stop, stepSize)
  local index = eval(ctx, start)
  local stopIndex = eval(ctx, stop)
  local size = eval(ctx, stepSize) or 1
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
end, nil, true)

local cycle = funcify(function(ctx, ...)
  local args = evalMany(ctx, { ... })

  local i = 0
  return iteratorSeq(function()
    i = i + 1
    if i > #args then
      i = 1
    end
    return args[i]
  end)
end, nil, true)

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
  local args = evalMany(ctx, { ... })

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
end, nil, true)

local count = funcify(function(ctx, seq)
  local mySeq = seqify(seq)
  local i = 0
  while mySeq.at(i + 1) ~= SEQ_DONE_SYMBOL do
    i = i + 1
  end

  return i
end, 1)

--

local upper = luaf (1) (string.upper)
local lower = luaf (1) (string.lower)

local strc = funcify(function(ctx, ...)
  local args = evalMany(ctx, { ... })
  local str = ""
  for k, v in ipairs(args) do
    str = str .. format(ctx, v)
  end

  return str
end, nil, true)

local str = funcify(function(ctx, value)
  return format(ctx, value)
end, 1)

local seqstr = funcify(function(ctx, value)
  local str = ""
  local mySeq = seqify(value)
  local i = 0
  while true do
    local nextValue = mySeq.at(i + 1)
    if nextValue == SEQ_DONE_SYMBOL then
      break
    end
    str = str .. format(ctx, nextValue)
    i = i + 1
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
    mtbl[eval(ctx, args[i])] = args[i + 1]
  end

  return mtbl
end, nil, true)

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

local function createBinder(typeSymbol)
  return function(name)
    return function(value)
      local bindValue = {
        [typeSymbol] = true,
        name = name,
        value = value
      }
      return setmetatable(bindValue, {
        __call = function(_, ...)
          return bindValue
        end
      })
    end
  end
end

local bind = createBinder(BIND_SYMBOL)
local bindval = createBinder(BIND_VALUE_SYMBOL)

local block = funcify(function(ctx, ...)
  local args = { ... }
  if #args == 0 then
    error("block: No arguments")
  end

  local strippedArgs = {}
  for k, v in ipairs(args) do
    table.insert(strippedArgs, removeEvalWrapper(v))
  end

  local lastValue = strippedArgs[#args]
  if (lastValue == "table") and (lastValue[BIND_SYMBOL] or lastValue[BIND_VALUE_SYMBOL]) then
    error("block: Last value of block cannot be a bind")
  end

  if #args == 1 then
    return eval(deriveScopeCtx(ctx), lastValue)
  end
  
  local bindOp = eval(ctx, method, "builtin.monad", ">>=")
  local nextOp = eval(ctx, method, "builtin.monad", ">>")

  lastValue = strippedArgs[#strippedArgs]
  for i = #args - 1, 1, -1 do
    local value = strippedArgs[i]
    if (type(value) == "table") and value[BIND_SYMBOL] then
      local vname = eval(ctx, value.name)
      local transformerFunction = wrapUserFunction(nil, { vname }, lastValue)
      lastValue = bindOp(removeEvalWrapper(value.value), transformerFunction)
    elseif (type(value) == "table") and value[BIND_VALUE_SYMBOL] then
      local vname = eval(ctx, value.name)
      local margs = { vname, removeEvalWrapper(value.value), lastValue }

      -- Small optimization to avoid nested with
      local nextArg = strippedArgs[i - 1]
      while nextArg and type(nextArg) == "table" and nextArg[BIND_VALUE_SYMBOL] do
        table.insert(margs, 1, nextArg.name)
        table.insert(margs, 2, nextArg.value)
        i = i - 1
        nextArg = strippedArgs[i - 1]
      end

      lastValue = with(table.unpack(margs))
    else
      lastValue = nextOp(value, lastValue)
    end
  end

  return eval(deriveScopeCtx(ctx), lastValue)
end, nil, true)

--

local prints = ioFuncify(function(ctx, ...)
  local str = ""
  for k, v in ipairs({...}) do
    str = str .. format(ctx, eval(ctx, v)) .. " "
  end
  print(str:sub(1, #str - 1))
end)

local getArgs = ioFuncify(function(ctx)
  return seqify(ctx.programArgs)
end, 0)

local getLine = ioFuncify(function(ctx)
  return io.read()
end, 0)

local asIO = funcify(function(ctx, ...)
  local args = { ... }
  return ioEffect(function()
    return eval(ctx, args[1], table.unpack(args, 2))
  end)
end, 1, true)

local function isFailure(tbl)
  return type(tbl) == "table"
    and tbl[INSTANTIATED_TYPE_SYMBOL]
    and tbl.type.name == "builtin.failure"
end

local function isErr(tbl)
  return type(tbl) == "table"
    and tbl[INSTANTIATED_TYPE_SYMBOL]
    and tbl.type.name == "builtin.err"
end

local try = funcify(function(ctx, innerMonad)
  return ioEffect(function()
    local ok, result = pcall(innerMonad.effect)
    if not ok then
      return eval(ctx, t.builtin.left(createErr(ctx, result)))
    end
    if isFailure(result) or isErr(result) then
      return eval(ctx, t.builtin.left(result))
    end
    return eval(ctx, t.builtin.right(result))
  end)
end, 1)

-- Like try, but preserves failure values
local try2 = funcify(function(ctx, innerMonad)
  return ioEffect(function()
    local ok, result = pcall(innerMonad.effect)
    if not ok then
      return eval(ctx, t.builtin.left(createErr(ctx, result)))
    end
    return eval(ctx, t.builtin.right(result))
  end)
end, 1)

--

local builtinLib = customMod "builtin"
  (defn ">>=" (method "monad" ">>="))
  (defn ">>" (method "monad" ">>"))
  (deftrait "alternative"
    (defmethod "|" (argsF "other")))
  (deftrait "monad"
    (extends "alternative")
    (defmethod ">>=" (argsF "transform"))
    (defmethod ">>" (argsF "next") (f">>=" (a.self) (defn "" (argsF "y") (a.next)))))
  (newtype "just" "a")
  (newtype "empty")
  (newtype "left" "a")
  (newtype "right" "b")
  (newtype "failure" "reason")
  (newtype "err" "reason" "callsites")
  (instance "failure" "alternative"
    (defmethod "|" (argsF "other") (a.other)))
  (instance "failure" "monad"
    (defmethod ">>=" (a.self)))
  (instance "err" "alternative"
    (defmethod "|" (argsF "other") (a.other)))
  (instance "err" "monad"
    (defmethod ">>=" (a.self)))
  (instance "empty" "alternative"
    (defmethod "|" (argsF "other") (a.other)))
  (instance "empty" "monad"
    (defmethod ">>=" (argsF "transform") (a.self)))
  (instance "just" "alternative"
    (defmethod "|" (argsF "other") (a.self)))
  (instance "just" "monad"
    (defmethod ">>=" (argsF "transform")
      (match (a.self) (t.just (a.value)) (only (a.transform) (a.value)))))
  (exportst "just" "empty" "left" "right" "failure" "err")
  (exportstr "alternative" "monad")
  (exportsf ">>=" ">>")

--

local environment = {
  exec = exec,
  run = run,
  void = void,
  wrap = wrap,
  unwrap = unwrap,
  only = only,
  undefined = undefined,
  errs = errs,
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
  loadprovider = loadprovider,
  modulename = modulename,
  submodule = submodule,
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
  strc = strc,
  str = str,
  seqstr = seqstr,
  num = num,
  tbl = tbl,
  withKeys = withKeys,
  newTbl = newTbl,
  keys = keys,
  values = values,
  bind = bind,
  bindval = bindval,
  block = block,
  getArgs = getArgs,
  getLine = getLine,
  asIO = asIO,
  try = try,
  try2 = try2,
}

local rtn = {}
local function install(env)
  for k, v in pairs(environment) do
    env[k] = v
  end
end
install(rtn)

local function printCallSite(collectedCallSites)
  collectedCallSites = seqToTable(collectedCallSites)
  print("---\nThe Funclu library reported an error. If you believe this is a Funclu bug, " ..
    "please report it at " .. ISSUES_URL .. "\n---")

  local hasCallSite = not not (pairs(collectedCallSites)(collectedCallSites))
  if not hasCallSite then
    print("No call site information available")
    return
  end
  print("Potentially Relevant Call Sites:")
  for _, callSite in ipairs(collectedCallSites) do
    print(callSite)
  end
end

local function execLine(ctx, line)
  eval(ctx, line)
  if ctx.functions["main"] then
    local result = eval(ctx, ctx.functions["main"])
    result = eval(ctx, result)
    while (isIoEffect(result)) do
      result = eval(ctx, result.effect())
    end
    if isFailure(result) then
      print("Program exited with failure: " .. result.argAt(1))
    end
    if isErr(result) then
      if not enableDebug then
        error(result.argAt(1), -1)
      end
      error(result)
    end
    return true
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
      local ok, err = pcall(execLine, ctx, line)
      if not ok then
        local errT = (isErr(err) and err) or eval(ctx, createErr(ctx, err))
        printCallSite(errT.argAt(2))
        error(errT.argAt(1), -1)
      end
      if err then return end
    else
      local done = execLine(ctx, line)
      if done then return end
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