import macros

macro defineEnum(typ: untyped, enumNotDistinct: bool = false, enumBaseTypeStr: string = "cint"): untyped =
  result = newNimNode(nnkStmtList)
  
  let enumType = newIdentNode(enumBaseTypeStr.strVal)
  if enumNotDistinct.boolVal:
    result.add quote do:
      type `typ`* = `enumType`
  else:
    # Enum mapped to distinct cint
    result.add quote do:
      type `typ`* = distinct `enumType`

    for i in ["+", "-", "*", "div", "mod", "shl", "shr", "or", "and", "xor", "<", "<=", "==", ">", ">="]:
      let
        ni = newIdentNode(i)
        typout = if i[0] in "<=>": newIdentNode("bool") else: typ # comparisons return bool
      if i[0] == '>': # cannot borrow `>` and `>=` from templates
        let
          nopp = if i.len == 2: newIdentNode("<=") else: newIdentNode("<")
        result.add quote do:
          proc `ni`*(x: `typ`, y: `enumType`): `typout` = `nopp`(y, x)
          proc `ni`*(x: `enumType`, y: `typ`): `typout` = `nopp`(y, x)
          proc `ni`*(x, y: `typ`): `typout` = `nopp`(y, x)
      else:
        result.add quote do:
          proc `ni`*(x: `typ`, y: `enumType`): `typout` {.borrow.}
          proc `ni`*(x: `enumType`, y: `typ`): `typout` {.borrow.}
          proc `ni`*(x, y: `typ`): `typout` {.borrow.}
      result.add quote do:
        proc `ni`*(x: `typ`, y: int): `typout` = `ni`(x, y.`enumType`)
        proc `ni`*(x: int, y: `typ`): `typout` = `ni`(x.`enumType`, y)

    let
      divop = newIdentNode("/")   # `/`()
      dlrop = newIdentNode("$")   # `$`()
      notop = newIdentNode("not") # `not`()
    result.add quote do:
      proc `divop`*(x, y: `typ`): `typ` = `typ`((x.float / y.float).`enumType`)
      proc `divop`*(x: `typ`, y: `enumType`): `typ` = `divop`(x, `typ`(y))
      proc `divop`*(x: `enumType`, y: `typ`): `typ` = `divop`(`typ`(x), y)
      proc `divop`*(x: `typ`, y: int): `typ` = `divop`(x, y.`enumType`)
      proc `divop`*(x: int, y: `typ`): `typ` = `divop`(x.`enumType`, y)

      proc `dlrop`*(x: `typ`): string {.borrow.}
      proc `notop`*(x: `typ`): `typ` {.borrow.}
