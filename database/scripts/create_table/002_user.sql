use what_to_eatDB ;

CREATE TABLE `user` (
  `id` int NOT NULL AUTO_INCREMENT,
  `email` varchar(45) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `password` varchar(45) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `username` varchar(45) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `sys_role` varchar(45) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `approved` int DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `email_UNIQUE` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='사용자'


INSERT INTO `user` (`id`,`email`,`password`,`username`,`sys_role`,`approved`) VALUES (1,'admin@trialinformatics.com','39dfa55283318d31afe5a3ff4a0e3253e2045e43','관리자','admin',1);
INSERT INTO `user` (`id`,`email`,`password`,`username`,`sys_role`,`approved`) VALUES (2,'henry.kye@trialinformatics.com','7110eda4d09e062aa5e4a390b0a572ac0d2c0220','계형일','user',1);
INSERT INTO `user` (`id`,`email`,`password`,`username`,`sys_role`,`approved`) VALUES (3,'jeongwoo.yang@trialinformatics.com','a7052947dd00ef73f6e0db8adf65c341e398e2b1','양정우','user',1);

