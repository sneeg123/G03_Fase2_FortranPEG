{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from '../parser/error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js'
}}

gramatica = _ prods:producciones+ _ {

    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }

    return prods
}

producciones = _ id:identificador _ alias:(literales)? _ "=" _ exprs:opciones (_";")? 
{ 
    ids.push(id);
    return new n.Produccion(id, exprs, alias);
}

opciones = expr:union rest:(_ "/" _ @union)*
{
    return new n.Opciones([expr, ...rest]);
}

union = expr:expresion rest:(_ @expresion !(_ literales? _ "=") )*
{
    return new n.Union([expr, ...rest]);
}

expresion = ("@")? _ id:$(identificador _ ":")?_ varios? _ expr:expresiones _ qty: ([?+*]/conteo)?
{
    return new n.Expresion(expr, id, qty);
}


//etiqueta = ("@")? _ id:identificador _ ":" (varios)?

varios = ("!"/"&"/"$")

expresiones  =  id:identificador 
                { 
                usos.push(id) 
                return new n.Identificador(id)
                }
                / expr:$literales caseI:"i"?
                {
                    return new n.Literal(expr.replace(/['"]/g, ''), caseI);
                }
                / "(" _ opciones _ ")"
                / chars:clase isCase:"i"? {
                    return new n.Clase(chars, isCase)
                }
                / "."
                / "!."

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo = "|" _ (numero / id:identificador) _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "|"
        / "|" _ (numero / id:identificador)? _ "," _ opciones _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "," _ opciones _ "|"

clase
    = "[" @contenidoClase+ "]"

contenidoClase
  = bottom:$[^\[\]] "-" top:$[^\[\]] 
  {
    return new n.Rango(bottom, top);
  }
  / $[^\[\]]

literales = '"' @stringDobleComilla* '"' 
            / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape


continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    

numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }


_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
