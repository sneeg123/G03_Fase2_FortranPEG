import Visitor from "./Visitor.js";

import { Literal, Rango } from "./CST.js";

export default class Tokenizer extends Visitor {
  generateTokenizer(grammar) {
    return `
module parser
    implicit none

contains

subroutine parse(input)
    character(len=:), intent(inout), allocatable :: input
    character(len=:), allocatable :: lexeme
    integer :: cursor
    cursor = 1  ! Inicializar cursor a 1
    lexeme = ""  
    do while (lexeme /= "EOF" .and. lexeme /= "ERROR")
        lexeme = nextSym(input, cursor)
        print *, lexeme
    end do
end subroutine parse

function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    character(len=:), allocatable :: substring
    integer :: i

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    ${grammar.map((produccion) => produccion.accept(this)).join("\n")}

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym

    subroutine to_lower(str)
        character(len=*), intent(inout) :: str
        integer :: i
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end subroutine to_lower

end module parser
        `;
  }

  visitProduccion(node) {
    return node.exprs.accept(this);
  }
  visitOpciones(node) {
    return node.exprs.map((node) => node.accept(this)).join("\n");
  }

  visitUnion(node) {
    if (node.exprs.length == 1) {
      return node.exprs.map((node) => node.accept(this)).join("\n");
    } else {
      let code = "";
      code += "i = cursor \n";

      //element.expr.isCase==null
      let n = 0;
      let nTabs = 0;
      let codeEnd = "";
      let lengthLexema = 0;
      let counter = 0;
      node.exprs.forEach((element) => {
        let tabs = "\t".repeat(n);
        if (element.expr instanceof Literal) {
          if (element.expr.isCase == null) {
            lengthLexema += element.expr.val.length;
            code += `${tabs}if ("${
              element.expr.val
            }" == input(cursor:cursor + ${
              element.expr.val.length - 1
            })) then\n`;
            code +=
              element.qty == "+" || element.qty == "*"
                ? `${tabs}do while ("${
                    element.expr.val
                  }" == input(cursor:cursor + ${
                    element.expr.val.length - 1
                  }))\n`
                : "";
            code += `${tabs}  cursor = cursor + ${element.expr.val.length}\n`; //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
            code +=
              element.qty == "+" || element.qty == "*" ? `${tabs}end do\n` : "";
            if (
              (element.qty == "?" || element.qty == "*") &&
              n != node.exprs.length - 1
            ) {
              code += `${tabs}else\n`;
              n++;
            }
            if (n == node.exprs.length - 1) {
              // -- TODO: Agregar variable temporal para ir guardando cada lexema
              // -- y no perder el valor de los lexemas anteriores
              console.log(element.qty);
              if (element.qty == "?" || element.qty == "*") {
                code += `${tabs}  allocate( character(len=${lengthLexema}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}else\n`;
                code += `${tabs}  allocate( character(len=${lengthLexema}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}end if\n`;
                code += codeEnd;
              } else {
                code += `${tabs}  allocate( character(len=${lengthLexema}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}end if\n`;
                code += codeEnd;
              }
            }
          } else {
            lengthLexema += element.expr.val.length;
            code += `substring = input(cursor:cursor + ${
              element.expr.val.length - 1
            })\n`;
            code += `call to_lower(substring)\n `;
            code += `${tabs}if ("${element.expr.val.toLowerCase()}" == substring) then\n`;
            code += `${tabs}  cursor = cursor + ${element.expr.val.length}\n`; //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
            if (n == node.exprs.length - 1) {
              // -- TODO: Agregar variable temporal para ir guardando cada lexema
              // -- y no perder el valor de los lexemas anteriores
              code += `${tabs}  allocate( character(len=${lengthLexema}) :: lexeme)\n`;
              code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
              code += `${tabs}  return\n`;
              code += `${tabs}end if\n`;
              code += codeEnd;
            }
          }
        } else {
          counter = counter + 1;
          if (element.expr.isCase == null) {
            if (element.expr.chars[0] instanceof Rango) {
              code += `${tabs}if (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}") then\n`;
              code += `${tabs}  cursor = cursor + 1 \n `;
              if (n == node.exprs.length - 1) {
                code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}end if\n`;
                code += codeEnd;
              }
            } else {
              code += `if (findloc([${element.expr.chars
                .map((char) => `"${char}"`)
                .join(", ")}], input(cursor:cursor), 1) > 0) then\n`;
              code += `${tabs}  cursor = cursor + 1 \n`;
              if (n == node.exprs.length - 1) {
                code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}end if\n`;
                code += codeEnd;
              }
            }
          } else {
            code += `substring = input(cursor:cursor)\n`;
            code += `call to_lower(substring)\n`;
            if (element.expr.chars[0] instanceof Rango) {
              code += `${tabs}if (substring >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= ${element.expr.chars[0].top.toLowerCase()}) then\n`;
              code += `${tabs}  cursor = cursor + 1\n `;
              if (n == node.exprs.length - 1) {
                code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}end if\n`;
                code += codeEnd;
              }
            } else {
              code += `if (findloc([${element.expr.chars
                .map((char) => `"${char.toLowerCase()}"`)
                .join(", ")}], substring, 1) > 0) then\n`;
              code += `${tabs}  cursor = cursor + 1\n `;
              if (n == node.exprs.length - 1) {
                code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
                code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
                code += `${tabs}  return\n`;
                code += `${tabs}end if\n`;
                code += codeEnd;
              }
            }
          }
        }

        //TODO: REVISAR SI FALTA ALGUN STATEMENT DE FORTRAN
        codeEnd = `${n == 0 ? "" : tabs}end if\n` + codeEnd;
        n++;
      });
      //console.log(code);
      return code;
    }
  }

  visitExpresion(node) {
    return node.expr.accept(this);
  }
  visitLiteral(node) {
    if (node.isCase == null) {
      return `
    if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then
        allocate( character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if
    `;
    } else {
      return `
    substring = input(cursor:cursor + ${node.val.length - 1})
    call to_lower(substring)  
    if ("${node.val.toLowerCase()}" == substring) then
        allocate( character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if
      `;
    }
  }

  generateCaracteres(chars, caseI) {
    if (chars.length === 0) return "";
    if (caseI == null) {
      return `
    if (findloc([${chars
      .map((char) => `"${char}"`)
      .join(", ")}], input(i:i), 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    } else {
      return `
    substring = input(i:i)
    call to_lower(substring)
    if (findloc([${chars
      .map((char) => `"${char.toLowerCase()}"`)
      .join(", ")}], substring, 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    }
  }

  visitClase(node) {
    return `
    i = cursor
    ${this.generateCaracteres(
      node.chars.filter((node) => typeof node === "string"),
      node.isCase
    )}
    ${node.chars
      .filter((node) => node instanceof Rango)
      .map((range) => range.accept(this, node.isCase))
      .join("\n")}
        `;
  }

  visitRango(node, isCase) {
    if (isCase == null) {
      return `
    if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    } else {
      return `
    substring = input(i:i)
    call to_lower(substring)
    if (substring >= "${node.bottom.toLowerCase()}" .and. substring <= "${node.top.toLowerCase()}") then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    }
  }
}
