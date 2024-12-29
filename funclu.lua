local ARGS_SYMBOL = {}
local ARG_SYMBOL = {}

--

local function cloneTable(tbl)
  local newTbl = {}
  for k, v in pairs(tbl) do
    newTbl[k] = v
  end
  return newTbl
end

--

local function eval(ctx, func, ...)
  local res = func
  if type(func) == "function" then
    local args = { ... }
    if #args > 0 then
      res = res(...)
    end
    res = res()(ctx)
  end
  if (type(res) == "table") and (res[ARG_SYMBOL] == true) then
    return ctx.args[res.name]
  end
  return res
end

local function funcify(func)
  local f
  f = function(args)
    return function(...)
      local margs = { ... }
      if #margs == 0 then
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

local function luaf(func)
  return funcify(function(ctx, ...)
    return func(...)
  end)
end

local function luaftbl(func)
  return funcify(function(ctx, ...)
    return { func(...) }
  end)
end

local exec = funcify(function(ctx, ...)
  local funcs = { ... }
  for k, v in ipairs(funcs) do
    eval(ctx, v)
  end
end)

local seq = funcify(function(ctx, ...)
  return { ... }
end)

local wrap = funcify(function(ctx, func)
  return func
end)

local unwrap = funcify(function(ctx, func)
  return eval(ctx, func) -- TODO: Is this right?
end)

local f = funcify(function(ctx, name, ...)
  local args = { ... }
  if not ctx.functions[name] then
    error("f: No function named " .. name)
  end
  if #args == 0 then
    return ctx.functions[name]()(ctx)
  else
    return ctx.functions[name](...)()(ctx)
  end
end)

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
  
  local wrappedFunc = funcify(function(ctx2, ...)
    local innerArgs = { ... }
    local namedArgs = {}
    for i = 1, #argList do
      namedArgs[argList[i]] = eval(ctx2, table.remove(innerArgs, 1))
    end
    local newCtx = cloneTable(ctx2)
    newCtx.args = cloneTable(ctx2.args)
    for k, v in pairs(namedArgs) do
      newCtx.args[k] = v
    end
    return eval(newCtx, func, table.unpack(innerArgs))
  end)
  if name ~= "" then
    ctx.functions[name] = wrappedFunc
  end

  return wrappedFunc
end)

local a = funcify(function(ctx, name)
  return { [ARG_SYMBOL] = true, name = name }
end)

local argsF = funcify(function(ctx, ...)
  return { [ARGS_SYMBOL] = true, args = { ... } }
end)

local function run(program)
  eval({
    functions = {},
    args = {}
  }, program)
end

--

local format
format = function(ctx, rawValue)
  local value = eval(ctx, rawValue)
  if type(value) == "table" then
    local result = "(seq "
    for k, v in ipairs(value) do
      result = result .. "(" .. format(ctx, v) .. ")"
      if k < #value then
        result = result .. " "
      end
    end
    return result .. ")"
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

local inc = add (1)
local dec = add (-1)
local neg = mul (-1)

--

local eq = funcify(function(ctx, ...)
  local args = { ... }
  local value = eval(ctx, args[1])
  for k, v in ipairs(args) do
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

local check = funcify(function(ctx, value, func1, func2)
  if eval(ctx, value) then
    return eval(ctx, func1)
  else
    return eval(ctx, func2)
  end
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
  local results = {}
  for k, v in ipairs(eval(ctx, args)) do
    table.insert(results, func(eval(ctx, v)))
  end
  return results
end)

local filter = funcify(function(ctx, func, args)
  local results = {}
  for k, v in ipairs(eval(ctx, args)) do
    local result = eval(ctx, v)
    if func(result) then
      table.insert(results, result)
    end
  end
end)

local foldl = funcify(function(ctx, func, acc, args)
  local result = eval(ctx, acc)
  for k, v in ipairs(eval(ctx, args)) do
    result = func(result, eval(ctx, v))
  end
  return result
end)

local foldr = funcify(function(ctx, func, acc, args)
  local result = eval(ctx, acc)
  for k, v in ipairs(eval(ctx, args)) do
    result = func(eval(ctx, v), result)
  end
  return result
end)

--

local environment = {
  exec = exec,
  seq = seq,
  wrap = wrap,
  unwrap = unwrap,
  f = f,
  defn = defn,
  a = a,
  args = argsF,
  run = run,
  luaf = luaf,
  luaftbl = luaftbl,
  add = add,
  sub = sub,
  mul = mul,
  div = div,
  inc = inc,
  dec = dec,
  neg = neg,
  eq = eq,
  lt = lt,
  gt = gt,
  lte = lte,
  gte = gte,
  neq = neq,
  check = check,
  prints = prints,
  map = map,
  filter = filter,
  foldl = foldl,
  foldr = foldr,
}

local rtn = {}
local function install(env)
  for k, v in pairs(environment) do
    env[k] = v
  end
end
install(rtn)
rtn.install = install

rtn.custom = {
  funcify = funcify,
  eval = eval,
}

return rtn