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
    if (node.exprs.length < 0) {
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
      let start = [];
      let codeStartt = "";
      let startFlag = false;
      node.exprs.forEach((element) => {
        let tabs = "\t".repeat(n);
        if (element.expr instanceof Literal) {
          if (element.expr.isCase == null) {
            if (element.qty == '?') {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                // start.push(element.expr.val);
                codeStartt == "" ? codeStartt += `input(cursor:cursor + ${element.expr.val.length - 1}) == "${element.expr.val}"` : codeStartt += ` .or. input(cursor:cursor + ${element.expr.val.length - 1}) == "${element.expr.val}"`;
              }
              code += `\tif ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then\n`
              code += `\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\tend if\n`
              }
            } else if (element.qty == '*') {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                start.push(element.expr.val);
              }
              code += `\tif ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then\n`
              code += `\t\tdo while ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1}))\n`
              code += `\t\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              code += `\t\tend do\n`
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\tend if\n`
              }
            } else if (element.qty == '+') {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                // start.push(element.expr.val);
                codeStartt == "" ? codeStartt += `input(cursor:cursor + ${element.expr.val.length - 1}) == "${element.expr.val}"` : codeStartt += ` .or. input(cursor:cursor + ${element.expr.val.length - 1}) == "${element.expr.val}"`;
                startFlag = true;
              }
              code += `\tif ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then\n`
              code += `\t\tdo while ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1}))\n`
              code += `\t\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              code += `\t\tend do\n`
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                //Si no cumple guardar ERROR en el lexema
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\telse\n`
                //Si no cumple guardar ERROR en el lexema
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              }
            } else {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                // start.push(element.expr.val);
                codeStartt == "" ? codeStartt += `input(cursor:cursor + ${element.expr.val.length - 1}) == "${element.expr.val}"` : codeStartt += ` .or. input(cursor:cursor + ${element.expr.val.length - 1}) == "${element.expr.val}"`;
                startFlag = true;
              }
              code += `\tif ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then\n`
              code += `\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\telse\n`
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              }
            }
            //CASE INSENSITIVE
          } else {
            // lengthLexema += element.expr.val.length;
            // if (!startFlag) {
            //   start.push(element.expr.val);
            //   startFlag = true;
            // }
            // code += `substring = input(cursor:cursor + ${element.expr.val.length - 1})\n`
            // code += `call to_lower(substring)\n `
            // code += `${tabs}if ("${element.expr.val.toLowerCase()}" == substring) then\n`
            // code += `${tabs}  cursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
            // if (n == node.exprs.length - 1) {
            //   // -- TODO: Agregar variable temporal para ir guardando cada lexema
            //   // -- y no perder el valor de los lexemas anteriores
            //   code += `${tabs}  allocate( character(len=${lengthLexema}) :: lexeme)\n`
            //   code += `${tabs}  lexeme = input(i:cursor -1 )\n`
            //   code += `${tabs}  return\n`
            //   code += `${tabs}end if\n`
            //   code += codeEnd;
            // }
            if (element.qty == '?') {

              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                start.push(element.expr.val);
              }
              code += `\tsubstring = input(cursor:cursor + ${element.expr.val.length - 1})\n`
              code += `\tcall to_lower(substring)\n`
              code += `print *, substring\n`;
              code += `\tif ("${element.expr.val.toLowerCase()}" == substring) then\n`
              code += `\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\tend if\n`
              }
            } else if (element.qty == '*') {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                start.push(element.expr.val);
              }
              code += `\tsubstring = input(cursor:cursor + ${element.expr.val.length - 1})\n`
              code += `\tcall to_lower(substring)\n`
              code += `print *, substring\n`;
              code += `\tif ("${element.expr.val.toLowerCase()}" == substring) then\n`
              code += `\t\tdo while ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1}))\n`
              code += `\t\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              code += `\t\tend do\n`
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\tend if\n`
              }
            } else if (element.qty == '+') {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                start.push(element.expr.val);
                startFlag = true;
              }
              code += `\tsubstring = input(cursor:cursor + ${element.expr.val.length - 1})\n`
              code += `\tcall to_lower(substring)\n`
              code += `print *, substring\n`;
              code += `\tif ("${element.expr.val.toLowerCase()}" == substring) then\n`
              code += `\t\tdo while ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1}))\n`
              code += `\t\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              code += `\t\tend do\n`
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                //Si no cumple guardar ERROR en el lexema
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\telse\n`
                //Si no cumple guardar ERROR en el lexema
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              }
            } else {
              lengthLexema += element.expr.val.length;
              if (!startFlag) {
                start.push(element.expr.val);
                startFlag = true;
              }
              code += `\tsubstring = input(cursor:cursor + ${element.expr.val.length - 1})\n`
              code += `\tcall to_lower(substring)\n`
              code += `print *, substring\n`;
              code += `\tif ("${element.expr.val.toLowerCase()}" == substring) then\n`
              code += `\t\tcursor = cursor + ${element.expr.val.length}\n` //CORRER CURSOR PARA LEER EN EL SIGUIENTE IF
              if (n == node.exprs.length - 1) {
                code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                code += `\t\tlexeme = input(i:cursor -1 )\n`
                code += `\t\treturn\n`
                code += `\telse\n`
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              } else {
                code += `\telse\n`
                code += `\t\tallocate( character(len=5) :: lexeme)\n`
                code += `\t\tlexeme = "ERROR"\n`
                code += `\t\treturn\n`
                code += `\tend if\n`
              }
            }
          }
          //TODO: REVISAR SI FALTA ALGUN STATEMENT DE FORTRAN
          // codeEnd = `${n == 0 ? "  " : tabs}end if\n` + codeEnd;
          // RANGOS Y CARACTERES
        } else {
          console.log("es un rango")
          lengthLexema += 1;
          counter += 1;
          if (element.expr.isCase == null || element.expr.isCase == undefined) {
            // if (element.expr.chars[0] instanceof Rango) {
            //   code += `${tabs}if (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}") then\n`;
            //   code += `${tabs}  cursor = cursor + 1 \n `;
            //   if (n == node.exprs.length - 1) {
            //     code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
            //     code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
            //     code += `${tabs}  return\n`;
            //     code += `${tabs}end if\n`;
            //     code += codeEnd;
            //   }
            // } else {
            // code += `if (findloc([${element.expr.chars
            //   .map((char) => `"${char}"`)
            //   .join(", ")}], input(cursor:cursor), 1) > 0) then\n`;
            //   code += `${tabs}  cursor = cursor + 1 \n`;
            //   if (n == node.exprs.length - 1) {
            //     code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
            //     code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
            //     code += `${tabs}  return\n`;
            //     code += `${tabs}end if\n`;
            //     code += codeEnd;
            //   }
            // }
            if (element.expr.chars[0] instanceof Rango) {
              if (element.qty == '?') {
                code += `\tif (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}") then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }
              } else if (element.qty == '*') {
                code += `\tif (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}") then\n`;
                code += `\t\tdo while (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}")\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }

              } else if (element.qty == '+') {
                code += `\tif (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}") then\n`;
                code += `\t\tdo while (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}")\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              } else {
                code += `\tif (input(cursor:cursor) >= "${element.expr.chars[0].bottom}"  .and. input(cursor:cursor) <= "${element.expr.chars[0].top}") then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              }
            } else {
              if (element.qty == '?') {
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char}"`)
                  .join(", ")}], input(cursor:cursor), 1) > 0) then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }
              } else if (element.qty == '*') {
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char}"`)
                  .join(", ")}], input(cursor:cursor), 1) > 0) then\n`;
                code += `\t\tdo while (findloc([${element.expr.chars
                  .map((char) => `"${char}"`)
                  .join(", ")}], input(cursor:cursor), 1) > 0)\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }

              } else if (element.qty == '+') {
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char}"`)
                  .join(", ")}], input(cursor:cursor), 1) > 0) then\n`;
                code += `\t\tdo while (findloc([${element.expr.chars
                  .map((char) => `"${char}"`)
                  .join(", ")}], input(cursor:cursor), 1) > 0)\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              } else {
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char}"`)
                  .join(", ")}], input(cursor:cursor), 1) > 0) then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              }
            }
          } else {
            // if (element.expr.chars[0] instanceof Rango) {
            //   code += `${tabs}if (substring >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= ${element.expr.chars[0].top.toLowerCase()}) then\n`;
            //   code += `${tabs}  cursor = cursor + 1\n `;
            //   if (n == node.exprs.length - 1) {
            //     code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
            //     code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
            //     code += `${tabs}  return\n`;
            //     code += `${tabs}end if\n`;
            //     code += codeEnd;
            //   }
            // } else {
            //   code += `if (findloc([${element.expr.chars
            //     .map((char) => `"${char.toLowerCase()}"`)
            //     .join(", ")}], substring, 1) > 0) then\n`;
            //   code += `${tabs}  cursor = cursor + 1\n `;
            //   if (n == node.exprs.length - 1) {
            //     code += `${tabs}  allocate( character(len=${counter}) :: lexeme)\n`;
            //     code += `${tabs}  lexeme = input(i:cursor -1 )\n`;
            //     code += `${tabs}  return\n`;
            //     code += `${tabs}end if\n`;
            //     code += codeEnd;
            //   }
            // }
            if (element.expr.chars[0] instanceof Rango) {
              if (element.qty == '?') {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (substring >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= "${element.expr.chars[0].top.toLowerCase()}") then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }
              } else if (element.qty == '*') {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (substring >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= "${element.expr.chars[0].top.toLowerCase()}") then\n`;
                code += `\t\tdo while (input(cursor:cursor) >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= "${element.expr.chars[0].top.toLowerCase()}")\n`;
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }

              } else if (element.qty == '+') {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (substring >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= "${element.expr.chars[0].top.toLowerCase()}") then\n`;
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\t\tdo while (input(cursor:cursor) >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= "${element.expr.chars[0].top.toLowerCase()}")\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              } else {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (substring >= "${element.expr.chars[0].bottom.toLowerCase()}"  .and. substring <= "${element.expr.chars[0].top.toLowerCase()}") then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              }
            } else {
              if (element.qty == '?') {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char.toLowerCase()}"`)
                  .join(", ")}], substring, 1) > 0) then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }
              } else if (element.qty == '*') {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char.toLowerCase()}"`)
                  .join(", ")}], substring, 1) > 0) then\n`;
                code += `\t\tdo while (findloc([${element.expr.chars
                  .map((char) => `"${char.toLowerCase()}"`)
                  .join(", ")}], substring, 1) > 0)\n`;
                code += `\t\tsubstring = input(cursor:cursor)\n`;
                code += `\t\tcall to_lower(substring)\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`
                  code += `\t\tlexeme = input(i:cursor -1 )\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\tend if\n`
                }

              } else if (element.qty == '+') {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char.toLowerCase()}"`)
                  .join(", ")}], substring, 1) > 0) then\n`;
                code += `\t\tdo while (findloc([${element.expr.chars
                  .map((char) => `"${char.toLowerCase()}"`)
                  .join(", ")}], substring, 1) > 0)\n`;
                code += `\t\tsubstring = input(cursor:cursor)\n`;
                code += `\t\tcall to_lower(substring)\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                code += `\t\tend do\n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              } else {
                code += `\tsubstring = input(cursor:cursor)\n`;
                code += `\tcall to_lower(substring)\n`;
                code += `\tif (findloc([${element.expr.chars
                  .map((char) => `"${char.toLowerCase()}"`)
                  .join(", ")}], substring, 1) > 0) then\n`;
                code += `\t\tcursor = cursor + 1 \n`;
                if (n == node.exprs.length - 1) {
                  code += `\t\tallocate( character(len=${lengthLexema}) :: lexeme)\n`;
                  code += `\t\tlexeme = input(i:cursor -1 )\n`;
                  code += `\t\treturn\n`;
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                } else {
                  code += `\telse\n`
                  code += `\t\tallocate( character(len=5) :: lexeme)\n`
                  code += `\t\tlexeme = "ERROR"\n`
                  code += `\t\treturn\n`
                  code += `\tend if\n`
                }
              }
            }
          }
        }
        n++;
      });
      let codeStart = "if (";
      start.forEach((element, index) => {
        codeStart += `input(cursor:cursor + ${element.length - 1}) == "${element}"`;
        if (index < start.length - 1) {
          codeStart += " .or. ";
        }
      });
      codeStart += ") then\n";
      if (start.length > 1) {
        code = codeStart + code;
        code += "end if\n";
      }
      console.log(code);
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
