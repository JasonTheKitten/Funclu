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

local function keyFunc(func)
  return setmetatable({}, {
    __index = function(_, key)
      return func(key)
    end,
    __call = function(_, ...)
      return func(...)
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
  return res
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
    else
      table.insert(appliedArgs, v)
    end
  end

  return table.unpack(appliedArgs)
end

local function funcify(func)
  local f
  f = function(args, collectedCallSites)
    return function(...)
      local margs = { ... }
      if margs[1] == EVAL_SYMBOL then
        local ctx = margs[2]
        local oldCallSites = ctx.debugRef[1]
        ctx.debugRef[1] = collectedCallSites
        local result = { func(ctx, applyArgs(ctx, args)) }
        ctx.debugRef[1] = oldCallSites
        return table.unpack(result)
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
          local callsite = err:sub(1, err:find(": Callsite Here") - 1)
          if callsite ~= "" then
            mCollectedCallSites[callsite] = true
          end
        end

        return f(allArgs, mCollectedCallSites)
      end
    end
  end

  return f({}, {})
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

local function evalAll(ctx, ...)
  local args = { ... }
  for i = 1, #args do
    args[i] = eval(ctx, args[i])
  end
  return table.unpack(args)
end

local function luaf(func)
  return funcify(function(ctx, ...)
    return func(evalAll(ctx, ...))
  end)
end

local function luaftbl(func)
  return funcify(function(ctx, ...)
    return tblSeq({ func(evalAll(ctx, ...)) })
  end)
end

local exec = funcify(function(ctx, ...)
  local funcs = { ... }
  for k, v in ipairs(funcs) do
    eval(ctx, v)
  end
end)

local wrap = funcify(function(ctx, func)
  return func
end)

local unwrap = funcify(function(ctx, func)
  return eval(ctx, func) -- TODO: Is this right?
end)

local f = keyFunc(funcify(function(ctx, name, ...)
  local args = { ... }
  if not ctx.functions[name] then
    error("f: No function named " .. name)
  end
  if #args == 0 then
    return ctx.functions[name](EVAL_SYMBOL, ctx)
  else
    return ctx.functions[name](...)(EVAL_SYMBOL, ctx)
  end
end))

local function wrapUserFunction(scopeCtx, argList, func)
  return funcify(function(ctx2, ...)
    local innerArgs = { ... }
    local namedArgs = {}
    for i = 1, #argList do
      namedArgs[argList[i]] = eval(ctx2, table.remove(innerArgs, 1))
    end
    -- TODO: What should be inherited from ctx, and what should be inherited from ctx2?
    local newCtx = cloneTable(scopeCtx)
    newCtx.args = cloneTable(scopeCtx.args)
    for k, v in pairs(namedArgs) do
      newCtx.args[k] = v
    end
    return eval(newCtx, func, table.unpack(innerArgs))
  end)
end

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
  
  local scopeCtx = cloneTable(ctx, true)
  local wrappedFunc = wrapUserFunction(scopeCtx, argList, func)
  if name ~= "" then
    ctx.functions[name] = wrappedFunc
    scopeCtx.functions[name] = wrappedFunc
  end

  return wrappedFunc -- TODO: This does not actually work as intended
end)

local a = keyFunc(funcify(function(ctx, name)
  return { [ARG_SYMBOL] = true, name = name }
end))

local argsF = funcify(function(ctx, ...)
  return { [ARGS_SYMBOL] = true, args = { ... } }
end)

local function createCtx(args, options)
  local myOptions = defaultTable(options or {}, {
    
  })
  return {
    functions = {},
    args = {},
    types = {},
    traits = {},
    debugRef = {},
    programArgs = args,
    disableArgumentResolution = false
  }
end

local function run(program, args, options)
  eval(createCtx(args, options), program)
end

--

local newtype = funcify(function(ctx, name, ...)
  local args = { ... }
  local eArgs = {}
  local firstArg = #args > 0 and eval(ctx, args[1]) or nil
  if firstArg and type(firstArg) == "table" and firstArg[ARGS_SYMBOL] then
    eArgs = args[1].args
  else
    table.insert(eArgs, firstArg)
    for i = 2, #args do
      table.insert(eArgs, eval(ctx, args[i]))
    end
  end

  local ntype = {
    [CUSTOM_TYPE_SYMBOL] = true,
    name = name,
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
    if aValue.type ~= bValue.type or aValue.name ~= bValue.name then
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
end)

local member = funcify(function(ctx, name, value)
  local eValue = eval(ctx, value)

  if type(eValue) == "table" and not eValue[INSTATIATED_TYPE_SYMBOL] then
    return eValue[name]
  end

  if type(eValue) ~= "table" then
    error("member: Not an instatiated type")
  end
  local ntype = eValue.type

  for i = 1, #ntype.args do
    if ntype.args[i] == name then
      return eValue.args[i]
    end
  end

  error("member: No member named " .. name)
end)

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
end)

local any = funcify(function(ctx, ...)
  return ANY_SYMBOL
end)

local typeConstructor = function(ctx, name, ...)
  local ntype = ctx.types[name]
  if not ntype then
    error("t: No type named " .. name)
  end

  local args = { ... }
  if #args ~= #ntype.args then
    error("t: Incorrect number of arguments")
  end

  return {
    [INSTATIATED_TYPE_SYMBOL] = true,
    type = ntype,
    args = args
  }
end
local t = keyFunc(funcify(typeConstructor))

--

local deftrait = funcify(function(ctx, name, ...)
  local args = { ... }
  local traitName = eval(ctx, name)
  local trait = {
    name = traitName,
    methods = {},
    extends = {}
  }
  for i = 1, #args do
    local arg = eval(ctx, args[i])
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
    local scopeCtx = cloneTable(ctx, true)
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
  }
end)

local instance = funcify(function(ctx, typeName, traitName, ...)
  local args = {...}
  local eTypeName = eval(ctx, typeName)
  local eTraitName = eval(ctx, traitName)
  local ntype = ctx.types[eTypeName]
  local trait = ctx.traits[eTraitName]
  
  if not ntype then
    error("instance: No type named " .. eTypeName)
  end
  if not trait then
    error("instance: No trait named " .. eTraitName)
  end
  if ntype.instances[eTraitName] then
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
    local arg = eval(ctx, args[i])
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

  ntype.instances[eTraitName] = instance
end)

local extends = funcify(function(ctx, name)
  local eName = eval(ctx, name)
  if not ctx.traits[eName] then
    error("extends: No trait named " .. eName)
  end
  return {
    [EXTENDS_SYMBOL] = true,
    name = eName
  }
end)

local method = funcify(function(ctx, traitName, value, methodName, ...)
  local eValue = eval(ctx, value)
  if not (type(eValue) == "table" and eValue[INSTATIATED_TYPE_SYMBOL]) then
    error("method: Not an instatiated type")
  end

  local eTraitName = eval(ctx, traitName)
  local trait = eValue.type.instances[eTraitName]
  if not trait then
    if ctx.traits[eTraitName] then
      error("method: Instance does not implement trait " .. eTraitName)
    else
      error("method: No trait named " .. eTraitName)
    end
  end

  local eMethodName = eval(ctx, methodName)
  local method = trait.methods[eMethodName]
  if not method then
    error("method: No method named " .. eMethodName)
  end

  return eval(ctx, method.wrappedFunc, value, ...)
end)

local isInstance = funcify(function(ctx, typeName, traitName)
  local eTypeName = eval(ctx, typeName)
  local eTraitName = eval(ctx, traitName)
  local ntype = ctx.types[eTypeName]
  local trait = ctx.traits[eTraitName]
  if not ntype then
    error("isInstance: No type named " .. eTypeName)
  end
  if not trait then
    error("isInstance: No trait named " .. eTraitName)
  end

  local instances = ctx.instances[eTypeName]
  if not instances then
    return false
  end

  local instance = instances[eTraitName]
  return instance and true or false
end)

--

local format
format = function(ctx, rawValue)
  local value = eval(ctx, rawValue)
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
    sum = sum + eval(ctx, v)
  end
  return sum
end)

local sub = funcify(function(ctx, ...)
  local args = { ... }
  local diff = eval(ctx, args[1])
  for i = 2, #args do
    diff = diff - eval(ctx, args[i])
  end
  return diff
end)

local mul = funcify(function(ctx, ...)
  local args = { ... }
  local product = 1
  for k, v in ipairs(args) do
    product = product * eval(ctx, v)
  end
  return product
end)

local div = funcify(function(ctx, ...)
  local args = { ... }
  local quotient = eval(ctx, args[1])
  for i = 2, #args do
    quotient = quotient / eval(ctx, args[i])
  end
  return quotient
end)

local mod = funcify(function(ctx, a, b)
  return eval(ctx, a) % eval(ctx, b)
end)

local inc = add (1)
local dec = add (-1)
local neg = mul (-1)

--

local eq = funcify(function(ctx, ...)
  local args = { ... }
  local value = eval(ctx, args[1])
  for i = 2, #args do
    local v = args[i]
    if value ~= eval(ctx, v) then
      return false
    end
  end
  return true
end)

local lt = funcify(function(ctx, a, b)
  return eval(ctx, a) < eval(ctx, b)
end)

local gt = funcify(function(ctx, a, b)
  return eval(ctx, a) > eval(ctx, b)
end)

local lte = funcify(function(ctx, a, b)
  return eval(ctx, a) <= eval(ctx, b)
end)

local gte = funcify(function(ctx, a, b)
  return eval(ctx, a) >= eval(ctx, b)
end)

local neq = funcify(function(ctx, a, b)
  return eval(ctx, a) ~= eval(ctx, b)
end)

--

local seq = funcify(function(ctx, ...)
  return tblSeq({ ... })
end)

local toseq = funcify(function(ctx, value)
  return seqify(eval(ctx, value))
end)

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
  for k, v in ipairs(args) do
    if not eval(ctx, v) then
      return false
    end
  end
  return true
end)

local or_ = funcify(function(ctx, ...)
  local args = { ... }
  for k, v in ipairs(args) do
    if eval(ctx, v) then
      return true
    end
  end
  return false
end)

local not_ = funcify(function(ctx, a)
  return not eval(ctx, a)
end)

local xor = funcify(function(ctx, a, b)
  return eval(ctx, a) ~= eval(ctx, b)
end)

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
end)

local with = funcify(function(ctx, ...)
  local args = {...}

  local newCtx = cloneTable(ctx)
  newCtx.args = cloneTable(ctx.args)
  for i = 1, #args, 2 do
    newCtx.args[args[i]] = eval(newCtx, args[i + 1])
  end

  local func = args[#args]
  return eval(newCtx, func)
end)

local prints = funcify(function(ctx, ...)
  local args = { ... }
  local str = ""
  for k, v in ipairs(args) do
    str = str .. format(ctx, v) .. " "
  end
  print(str:sub(1, #str - 1))
end)

--

local map = funcify(function(ctx, func, args)
  local cache = {}
  local initialVal = eval(ctx, args)
  local myArgs = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      if not cache[i] then
        local value = eval(ctx, myArgs.at(i))
        if value == SEQ_DONE_SYMBOL then
          return SEQ_DONE_SYMBOL
        end
        cache[i] = eval(ctx, func, value)
      end
      return cache[i]
    end
  }

  return seqToOriginal(newSeq, initialVal)
end)

local filter = funcify(function(ctx, func, args)
  local cache, index, innerIndex = {}, 1, 1
  local initialVal = eval(ctx, args)
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
end)

local foldl = funcify(function(ctx, func, acc, args)
  local result = eval(ctx, acc)
  for k, v in ipairs(seqToTable(eval(ctx, args))) do
    result = func(result, eval(ctx, v))
  end
  return result
end)

local foldr = funcify(function(ctx, func, acc, args)
  local result = eval(ctx, acc)
  for k, v in ipairs(seqToTable(eval(ctx, args))) do
    result = func(eval(ctx, v), result)
  end
  return result
end)

local iterator = funcify(function(ctx, start, stepSize, numSteps)
  local index = eval(ctx, start) or 1
  local size = eval(ctx, stepSize) or 1
  local steps = eval(ctx, numSteps)
  local i = 0
  return iteratorSeq(function()
    i = i + 1
    if steps and i > steps then
      return SEQ_DONE_SYMBOL
    end
    local value = index
    index = index + size
    return value
  end)
end)

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
  end)
end)

local cycle = funcify(function(ctx, ...)
  local args = {...}
  local eArgs = {}
  for k, v in ipairs(args) do
    table.insert(eArgs, eval(ctx, v))
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

local take = funcify(function(ctx, n, args)
  local maxN = eval(ctx, n)
  local initialVal = eval(ctx, args)
  local myArgs = seqify(initialVal)
  local i = 0
  local newSeq = iteratorSeq(function()
    i = i + 1
    if i > maxN then
      return SEQ_DONE_SYMBOL
    end
    return myArgs.at(i)
  end)

  return seqToOriginal(newSeq, initialVal)
end)

local skip = funcify(function(ctx, n, args)
  local minN = eval(ctx, n)
  local initialVal = eval(ctx, args)
  local mySeq = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      return mySeq.at(i + minN)
    end
  }

  return seqToOriginal(newSeq, initialVal)
end)

local head = funcify(function(ctx, args)
  return seqify(eval(ctx, args)).at(1)
end)

local tail = funcify(function(ctx, args)
  local initialVal = eval(ctx, args)
  local mySeq = seqify(initialVal)
  local newSeq = {
    [SEQ_SYMBOL] = true,
    at = function(i)
      return mySeq.at(i + 1)
    end
  }
  return seqToOriginal(newSeq, initialVal)
end)

local subseq = funcify(function(ctx, start, num, args)
  local initialVal = eval(ctx, args)
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
end)

local splice = funcify(function(ctx, start, num, args)
  local initialVal = eval(ctx, args)
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
end)

local concat = funcify(function(ctx, ...)
  local args = {}
  for k, v in ipairs({ ... }) do
    table.insert(args, eval(ctx, v))
  end
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

local count = funcify(function(ctx, args)
  local mySeq = seqify(eval(ctx, args))
  local i = 0
  while mySeq.at(i + 1) ~= SEQ_DONE_SYMBOL do
    i = i + 1
  end

  return i
end)

--

local upper = luaf (string.upper)
local lower = luaf (string.lower)

local str = funcify(function(ctx, value)
  local eValue = eval(ctx, value)
  if type(eValue) == "string" then
    return eValue
  end
  if type(eValue) == "table" and eValue[SEQ_SYMBOL] then
    return table.concat(seqToTable(eValue))
  end
  -- TODO: Is this a good default?
  return format(ctx, eValue)
end)

--

local num = funcify(function(ctx, value)
  local eValue = eval(ctx, value)
  local asNum = tonumber(eValue)
  if not asNum then
    error("num: Cannot convert to number")
  end
  return asNum
end)

--

local tbl = funcify(function(ctx, value)
  local eValue = eval(ctx, value)
  if eValue == nil then
    return {}
  end
  if type(eValue) == "table" and eValue[SEQ_SYMBOL] then
    return seqToTable(eValue)
  end
  if type(eValue) == "table" then
    return eValue
  end
  error("tbl: Don't know how to convert to table")
end)

local withKeys = funcify(function(ctx, value, ...)
  local args = { ... }
  local mtbl = cloneTable(eval(ctx, value), true)
  for i = 1, #args, 2 do
    local key = eval(ctx, args[i])
    local value = eval(ctx, args[i + 1])
    mtbl[key] = value
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
end)

local values = funcify(function(ctx, value)
  local eValue = eval(ctx, value)
  local valueList = {}
  for k, v in pairs(eValue) do
    table.insert(valueList, v)
  end
  return tblSeq(valueList)
end)

--

local getArgs = funcify(function(ctx)
  return seqify(ctx.programArgs)
end)

--

local environment = {
  exec = exec,
  wrap = wrap,
  unwrap = unwrap,
  f = f,
  defn = defn,
  a = a,
  args = argsF,
  run = run,
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
  local func
  func = function(line)
    if enableDebug then
      ctx.debugRef = {{}}
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

rtn.enableDebug = function(b)
  enableDebug = b ~= false
  return rtn
end

rtn.custom = {
  funcify = funcify,
  eval = eval,
}

return rtn