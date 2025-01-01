local ANY_SYMBOL = {}
local ARGS_SYMBOL = {}
local ARG_SYMBOL = {}
local CUSTOM_TYPE_SYMBOL = {}
local EVAL_SYMBOL = {}
local INSTATIATED_TYPE_SYMBOL = {}
local SEQ_SYMBOL = {}
local SEQ_DONE_SYMBOL = {}

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
  if type(func) == "function" then
    local args = { ... }
    if #args > 0 then
      res = res(...)
    end
    res = res(EVAL_SYMBOL)(ctx)
  end
  if (type(res) == "table") and (res[ARG_SYMBOL] == true) and not ctx.disableArgumentResolution then
    return ctx.args[res.name]
  end
  return res
end

local function funcify(func)
  local f
  f = function(args)
    return function(...)
      local margs = { ... }
      if margs[1] == EVAL_SYMBOL then
        return function(ctx) return func(ctx, table.unpack(args)) end
      else
        local allArgs = {}
        for k, v in ipairs(args) do
          table.insert(allArgs, v)
        end
        for k, v in ipairs(margs) do
          table.insert(allArgs, v)
        end

        return f(allArgs)
      end
    end
  end

  return f({})
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

local function iteratorSeq(it)
  local cache, i = {}, 1
  return {
    [SEQ_SYMBOL] = true,
    next = function()
      local value = it()
      if value == SEQ_DONE_SYMBOL then
        return SEQ_DONE_SYMBOL
      end
      cache[i] = value
      i = i + 1
      return value
    end,
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

local function seqToTable(seq)
  if not seq[SEQ_SYMBOL] then
    return seq -- Already in normal table form
  end

  local result, running, i = {}, true, 1
  while running do
    local value = seq.at(i)
    i = i + 1
    if value == SEQ_DONE_SYMBOL then
      running = false
    else
      table.insert(result, value)
    end
  end
  return result
end

local function seqify(tbl)
  if not tbl[SEQ_SYMBOL] then
    return tblSeq(tbl)
  end
  return tbl
end

--

local function luaf(func)
  return funcify(function(ctx, ...)
    return func(...)
  end)
end

local function luaftbl(func)
  return funcify(function(ctx, ...)
    return tblSeq({ func(...) })
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
    return ctx.functions[name](EVAL_SYMBOL)(ctx)
  else
    return ctx.functions[name](...)(EVAL_SYMBOL)(ctx)
  end
end))

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
  local wrappedFunc = funcify(function(ctx2, ...)
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

local function createCtx()
  return {
    functions = {},
    args = {},
    types = {},
    traits = {},
    disableArgumentResolution = false
  }
end

local function run(program)
  eval(createCtx, program)
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
    args = eArgs
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
  if not (type(eValue) == "table" and eValue[INSTATIATED_TYPE_SYMBOL]) then
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
  local cache, index = {}, 1
  local myArgs = seqify(eval(ctx, args))
  return {
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
end)

local filter = funcify(function(ctx, func, args)
  local cache, index, innerIndex = {}, 1, 1
  local myArgs = seqify(eval(ctx, args))

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

  return {
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

local iterator = funcify(function(ctx)
  local index = 0
  return iteratorSeq(function()
    index = index + 1
    return index
  end)
end)

local take = funcify(function(ctx, n, args)
  local maxN = eval(ctx, n)
  local myArgs = seqify(eval(ctx, args))
  local i = 0
  return iteratorSeq(function()
    i = i + 1
    if i > maxN then
      return SEQ_DONE_SYMBOL
    end
    return myArgs.at(i)
  end)
end)

local head = funcify(function(ctx, args)
  return eval(ctx, args).at(1)
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
  take = take,
  head = head,
}

local rtn = {}
local function install(env)
  for k, v in pairs(environment) do
    env[k] = v
  end
end
install(rtn)

local function installAndRun(env)
  install(env)
  local ctx = createCtx()
  local func
  func = function(line)
    eval(ctx, line)
    return func
  end

  return func
end
rtn.install = installAndRun

rtn.custom = {
  funcify = funcify,
  eval = eval,
}

return rtn