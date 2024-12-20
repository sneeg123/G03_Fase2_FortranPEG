module Tokenizer
    implicit none
    public
    type, public :: Token
        character(len=:), allocatable :: Tipo
        character(len=:), allocatable :: Lexema
    end type Token
contains
    function nextSym(input, cursor) result(Tokens)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        type(Token), allocatable :: Tokens
        character(len=:), allocatable :: substring 
        integer :: start

        allocate(Tokens)


        !Detecta foo con case insesitive
        substring = input(cursor:cursor + 2)
        call to_lower(substring)
        if (substring == 'foo') then
            allocate(character(len=3) :: Tokens%Lexema)
            Tokens%Lexema = input(cursor:cursor + 2)
            allocate(character(len=3) :: Tokens%Tipo)
            Tokens%Tipo = "integer"
            cursor = cursor + 3
            return 
        end if

        ! Strign de bar
        if ("bar" == input(cursor:cursor + 2)) then
            allocate(character(len=3) :: Tokens%Lexema)
            Tokens%Lexema = input(cursor:cursor + 2)
            allocate(character(len=3) :: Tokens%Tipo)
            Tokens%Tipo = "integer"
            cursor = cursor + 3
            return 
        end if

        !Detecta solo un de espacio en blanco
        if (input(cursor:cursor) == ' ') then
            allocate(character(len=1) :: Tokens%Lexema)
            Tokens%Lexema = ' '
            allocate(character(len=5) :: Tokens%Tipo)
            Tokens%Tipo = 'White_space'
            cursor = cursor + 1
            return 
        end if

       

        ! Detecta uno o más caracteres de un rango de 0-9
        if (iachar(input(cursor:cursor)) >= iachar('0') .and. iachar(input(cursor:cursor)) <= iachar('9')) then
            
            start = cursor
            do while (cursor <= len(input) .and. iachar(input(cursor:cursor)) >= iachar('0') .and. &
                        iachar(input(cursor:cursor)) <= iachar('9'))
                cursor = cursor + 1
            end do
            allocate(character(len=cursor-start) :: Tokens%Lexema)
            Tokens%Lexema = input(start:cursor-1)
            allocate(character(len=6) :: Tokens%Tipo)
            Tokens%Tipo = 'digit'
        return 
    end if

       ! Detecta identificadores que comienzan con [_a-zA-Z] y siguen con [a-zA-Z0-9_]
        substring = input(cursor:cursor)
        call to_lower(substring)
        if ((iachar(substring) >= iachar('a') .and. iachar(substring) <= iachar('z')) .or. substring == '_') then
            start = cursor
            
                do while (cursor <= len(input) .and. &
                    ((iachar(substring) >= iachar('a') .and. &
                    iachar(substring) <= iachar('z')) .or. &
                    (iachar(input(cursor:cursor)) >= iachar('0') .and. &
                    iachar(input(cursor:cursor)) <= iachar('9')) .or. &
                    input(cursor:cursor) == '_'))
                    cursor = cursor + 1
                    substring = input(cursor:cursor)
            end do
            allocate(character(len=cursor-start) :: Tokens%Lexema)
            Tokens%Lexema = input(start:cursor-1)
            allocate(character(len=2) :: Tokens%Tipo)
            Tokens%Tipo = 'ID'
            return
        end if
        
        print *, "error lexico en col ", cursor, ', "' // input(cursor:cursor) // '"'
        

        
    end function nextSym

    !Funcio para pasar strin o caracter a minuscula
    subroutine to_lower(str)
        character(len=*), intent(inout) :: str
        integer :: i
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end subroutine to_lower

end module Tokenizer