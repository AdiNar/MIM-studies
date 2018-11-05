qemu-system-x86_64 -curses -drive file=minix.img -localtime -net user,hostfwd=tcp::16000-:22 -net nic,model=virtio -m 1024M

