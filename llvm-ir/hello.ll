@.str = private constant [14 x i8] c"Hello, world!\00"

define i32 @main() {
entry:
    %0 = call i32 @puts(i8* getelementptr inbounds (i8*, [14 x i8]* @.str, i32 0))
    ret i32 0
}

declare i32 @puts(i8*)
