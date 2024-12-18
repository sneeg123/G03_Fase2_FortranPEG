program MAIN
    use Tokenizer
    implicit none

    character(len=100) :: input
    integer :: cursor
    type(Token), allocatable :: myToken

    ! Inicializar la cadena de entrada y el cursor
    input = "foo  Foo bar barbarbar 0 1 256 a b azdfa ADSFADS"
    cursor = 1

    ! Bucle para analizar la cadena de entrada
    do while (cursor <= len_trim(input))
        myToken = nextSym(input, cursor)
        if (allocated(myToken%Lexema)) then
            print *, "Token encontrado: ", myToken%Lexema, " de tipo: ", myToken%Tipo
        else
            exit
        end if
    end do

end program MAIN