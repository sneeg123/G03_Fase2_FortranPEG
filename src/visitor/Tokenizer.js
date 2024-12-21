import Visitor from "./Visitor.js";

import { Rango } from "./CST.js";

export default class Tokenizer extends Visitor {
  generateTokenizer(grammar) {
    return `
module tokenizer
implicit none

contains
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
end module tokenizer 
        `;
  }

  visitProduccion(node) {
    return node.exprs.accept(this);
  }
  visitOpciones(node) {
    return node.exprs.map((node) => node.accept(this)).join("\n");
  }
  visitUnion(node) {
    return node.exprs.map((node) => node.accept(this)).join("\n");
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
    `}
    else {
      return `
    substring = input(cursor:cursor + ${node.val.length - 1})
    call to_lower(substring)  
    if ("${node.val.toLowerCase()}" == substring) then
        allocate( character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if
      `
    };
    
  }

  generateCaracteres(chars,caseI) {
    if (chars.length === 0) return "";
    if (caseI == null){
    return `
    if (findloc([${chars
      .map((char) => `"${char}"`)
      .join(", ")}], input(i:i), 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
      }else{
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
      node.chars.filter((node) => typeof node === "string"),node.isCase
    )}
    ${node.chars
      .filter((node) => node instanceof Rango)
      .map((range) => range.accept(this,node.isCase))
      .join("\n")}
        `;
  }

  visitRango(node,isCase) {
    if (isCase == null){
    return `
    if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    }else{
    return `
    substring = input(i:i)
    call to_lower(substring)
    if (substring >= "${node.bottom.toLowerCase()}" .and. substring <= "${node.top.toLowerCase()}") then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    };
  }
}
