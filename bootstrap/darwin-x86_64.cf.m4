�Copyright 2022,2023 Eric Sessoms / MIT License

�include(`10_intro.cf')
�include(`20_x86_64.cf')

�darwin-x86_64 system call numbers
�---------------------------------
�sys_exit      �1 �sys �$2000000 + ;
�sys_read      �3 sys �;
�sys_write     �4 sys �;
�sys_close     �6 sys �;
�sys_ioctl    �54 sys �;
�sys_msync    �65 sys �;
�sys_munmap   �73 sys �;
�sys_mprotect �74 sys �;
�sys_socket   �97 sys �;
�sys_connect  �98 sys �;
�sys_mmap    �197 sys �;
�sys_lseek   �199 sys �;
�sys_openat  �463 sys �;

�include(`30_darwin.cf')
�include(`35_bsd.cf')
�include(`40_posix.cf')
�include(`50_bootstrap.cf')
