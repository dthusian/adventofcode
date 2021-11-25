RUNFILE=$1

if [[ -z $RUNFILE ]] ; then
  echo "No file specified"
  exit 1
fi

exit_handler() {
  rm -f common.o
  rm -f $RUNFILE.o
}

set -e
trap exit_handler EXIT

rm -f run.elf
nasm -f elf64 common.asm
nasm -f elf64 $1.asm
ld --dynamic-linker=/lib64/ld-linux-x86-64.so.2 -o run.elf $1.o common.o -lc
