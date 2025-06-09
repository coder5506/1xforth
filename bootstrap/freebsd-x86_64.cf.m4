�Copyright 2022,2023 Eric Sessoms / MIT License

�include(`10_intro.cf')
�include(`20_x86_64.cf')

�freebsd-x86_64 system call numbers
�----------------------------------
�sys_exit      �1 ;
�sys_read      �3 ;
�sys_write     �4 ;
�sys_close     �6 ;
�sys_ioctl    �54 ;
�sys_msync    �65 ;
�sys_munmap   �73 ;
�sys_mprotect �74 ;
�sys_socket   �97 ;
�sys_connect  �98 ;
�sys_mmap    �477 ;
�sys_lseek   �478 ;
�sys_openat  �499 ;

�include(`30_freebsd.cf')
�include(`35_bsd.cf')
�include(`40_posix.cf')
�include(`50_bootstrap.cf')
