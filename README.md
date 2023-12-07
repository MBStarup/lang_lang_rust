cargo run -q -- > ./a.asm && nasm -felf64 ./a.asm && ld ./a.o && ./a.out || echo $?
