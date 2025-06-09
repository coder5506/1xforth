áCopyright 2022,2023 Eric Sessoms / MIT License

áinclude(`10_intro.cf')
áinclude(`20_x86_64.cf')

álinux-x86_64 system call numbers
á--------------------------------
Åsys_read      Ç0 ;
Åsys_write     Ç1 ;
Åsys_close     Ç3 ;
Åsys_lseek     Ç8 ;
Åsys_mmap      Ç9 ;
Åsys_mprotect Ç10 ;
Åsys_munmap   Ç11 ;
Åsys_brk      Ç12 ;
Åsys_ioctl    Ç16 ;
Åsys_mremap   Ç25 ;
Åsys_msync    Ç26 ;
Åsys_socket   Ç41 ;
Åsys_connect  Ç42 ;
Åsys_exit     Ç60 ;
Åsys_openat  Ç257 ;

áinclude(`30_linux.cf')
áinclude(`40_posix.cf')
áinclude(`50_bootstrap.cf')
