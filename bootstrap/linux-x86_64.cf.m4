�Copyright 2022,2023 Eric Sessoms / MIT License

�include(`10_intro.cf')
�include(`20_x86_64.cf')

�linux-x86_64 system call numbers
�--------------------------------
�sys_read      �0 ;
�sys_write     �1 ;
�sys_close     �3 ;
�sys_lseek     �8 ;
�sys_mmap      �9 ;
�sys_mprotect �10 ;
�sys_munmap   �11 ;
�sys_brk      �12 ;
�sys_ioctl    �16 ;
�sys_mremap   �25 ;
�sys_msync    �26 ;
�sys_socket   �41 ;
�sys_connect  �42 ;
�sys_exit     �60 ;
�sys_openat  �257 ;

�include(`30_linux.cf')
�include(`40_posix.cf')
�include(`50_bootstrap.cf')
