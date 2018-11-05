# Adrian Naruszko

function list-qemu {
    echo $(top -n 1 -b | grep 'qemu' | awk '{print $1}')
}

function kill-qemu {
    for pid in $(list-qemu); do
	echo 'killing previous machine' $pid
	kill -9 $pid
    done
}

function gently-poweroff {
    sshpass -p 'root' ssh -p 16000 root@localhost "poweroff"
}

LAUNCH="qemu-system-x86_64 -drive file=minix.img -localtime -net user,hostfwd=tcp::16000-:22 -net nic,model=virtio -m 1024M"

function user-launch {
        gnome-terminal -x "bash" -c "qemu-system-x86_64 -drive file=minix.img -localtime -net user,hostfwd=tcp::16000-:22 -net nic,model=virtio 
-m 1024M; sleep 10"
}

function launch {
	qemu-system-x86_64 -curses -drive file=minix.img -localtime -net user,hostfwd=tcp::16000-:22 -net nic,model=virtio -m 1024M
-nographic
}

function start {
	kill-qemu
	restore
	launch &
	sleep 1
}

function connect {
	ssh -p 16000 root@localhost
}

function deploy {
    COPY_ORIG="dd bs=512 count=1 if=/dev/c0d0 of=./orig_bootloader"
    PREPARE="nasm bootloader.asm -o bootloader"
    INJECT_NEW="dd if=./bootloader of=/dev/c0d0"
    INJECT_ORIG="dd seek=1 bs=512 if=./orig_bootloader of=/dev/c0d0 conv=notrunc"
    COPY_FULL_NEW="dd bs=512 count=1 if=/dev/c0d0 of=./full_bootloader"
    INJECT_TWICE="dd seek=1 bs=512 if=./full_bootloader of=/dev/c0d0 conv=notrunc"
    SCRIPT="${COPY_ORIG}; ${PREPARE};"
#${INJECT_ORIG};"

#OFFSET=10
#INJECT_ORIG="dd seek=${OFFSET} bs=1 if=./orig_bootloader of=./bootloader conv=notrunc"

#COPY_NEW_BOOT="dd bs=512 if=./bootloader of=/dev/c0d0"

#${INJECT_ORIG}"

    sshpass -p 'root' scp -P 16000 ./files/bootloader.asm root@localhost:/root

    sshpass -p 'root' ssh -p 16000 root@localhost "${SCRIPT}"

    sshpass -p 'root' ssh -p 16000 root@localhost "${INJECT_NEW}"

    sshpass -p 'root' ssh -p 16000 root@localhost "${COPY_FULL_NEW}"
    sshpass -p 'root' ssh -p 16000 root@localhost "${INJECT_ORIG}"
    #sshpass -p 'root' ssh -p 16000 root@localhost "${INJECT_TWICE}"
}

function reboot_qemu {
    RBT="reboot"
    sshpass -p 'root' ssh -p 16000 root@localhost "${COPY_NEW_BOOT}"
}

function check-compile {
        nasm files/bootloader.asm -o tmp
	if [ $? -ne 0 ]; then
	    rm -f tmp
	    return 1
	fi
	rm -f tmp
	return 0
}

function check-compile-and-exit-on-error {
    if check-compile; then
	echo compilation successful
    else
	echo compile error
	exit 1
    fi
}

function git-commit {
    git add files/*
    git add run.sh
    number="$(git rev-list --count HEAD)"
    number=$((number+1))
    echo commiting version $number
    git commit -m "#${number}" >/dev/null
}

function run-no-reboot {
    check-compile-and-exit-on-error
    start
    echo 'waiting for qemu...'
    deploy
    echo 'qemu online, code deployed'
    connect
}

function run {
        check-compile-and-exit-on-error
        git-commit
	start
	echo 'waiting for qemu...'
	deploy
	echo 'qemu online, code deployed'
	kill-qemu
	echo 'qemu turned off'
	user-launch
}

function create {
        git init
	rm minix.img
        qemu-img create -f qcow2 -o backing_file=~/../../PUBLIC/SO/scenariusze/4/minix.img minix.img
}

function backup {
	cp minix.img backup
	echo "Backup done"
}

function restore {
	cp backup minix.img
	echo "Backup restored"
}

case "$1" in
    'backup' ) backup ;;
    'start' ) user-launch ;;
    'connect' ) connect ;;
    'create' ) create ;;
    'run' ) run ;;
    'list-qemu' ) list-qemu ;;
    'kill-qemu' ) kill-qemu ;;
    'show' ) run-no-reboot ;;
esac

if [[ "$1" == "files" ]]; then
	scp -P 16000 files/* root@localhost:/
fi

