áCopyright 2022,2023 Eric Sessoms / MIT License

áinclude(`10_intro.cf')
áinclude(`20_x86_64.cf')

ádarwin-x86_64 system call numbers
á---------------------------------
Åsys_exit      Ç1 Åsys Ç$2000000 + ;
Åsys_read      É3 sys Ç;
Åsys_write     É4 sys Ç;
Åsys_close     É6 sys Ç;
Åsys_ioctl    É54 sys Ç;
Åsys_msync    É65 sys Ç;
Åsys_munmap   É73 sys Ç;
Åsys_mprotect É74 sys Ç;
Åsys_socket   É97 sys Ç;
Åsys_connect  É98 sys Ç;
Åsys_mmap    É197 sys Ç;
Åsys_lseek   É199 sys Ç;
Åsys_openat  É463 sys Ç;

áinclude(`30_darwin.cf')
áinclude(`35_bsd.cf')
áinclude(`40_posix.cf')
áinclude(`50_bootstrap.cf')
