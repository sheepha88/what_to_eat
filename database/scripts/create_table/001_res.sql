use what_to_eatDB ;

CREATE TABLE `res` (
  `id` int NOT NULL AUTO_INCREMENT,
  `res_name` varchar(45) COLLATE utf8mb4_unicode_ci NOT NULL,
  `category` varchar(45) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `menu` text CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `rating_naver` float DEFAULT NULL,
  `url_naver` text CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci,
  `distance` int DEFAULT NULL,
  `deleted` int NOT NULL DEFAULT 0,
  PRIMARY KEY (`id`),
  UNIQUE KEY `res_name_UNIQUE` (`res_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='음식점 정보'


INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (1,'오봉집','한식','{\"보쌈정식\" : 10500, \"해물칼국수\" : 9000, \"낙지볶음\" : 14500}',NULL,'https://naver.me/5hgvwkzj',276,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (2,'간코','일식','{\"돈까스\" : 11000 , \"카레\" : 9500}',4.73,'https://naver.me/5QFd2UNp',274,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (3,'버거킹','양식','{\"햄버거\" : 7500}',4.41,'https://naver.me/FDX1B62d',138,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (4,'avo','양식','{\"포케\" : 14500}',4.2,'https://naver.me/xFr2VfHM',338,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (5,'길우동','한식,기타','{\"김밥\" : 4000,\"우동\" : 6000,\"오징어덮밥\" : 9000,\"잔치국수\" : 7000}',4.48,'https://naver.me/xmrOMG8e',365,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (6,'청와옥','한식','{\"순대국\" : 9000,\"오징어숯불구이\" : 9900,\"수육\" : 25000}',4.45,'https://naver.me/xyUjU5Kf',451,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (7,'코이라멘','일식','{\"돈코츠라멘\" : 9500,\"차슈덮밥\" : 9500,\"마제소바\" : 10500,\"튀김교자\" : 6000}',4.46,'https://naver.me/5DH335VY',444,0);
INSERT INTO `res` (`id`,`res_name`,`category`,`menu`,`rating_naver`,`url_naver`,`distance`,`deleted`) VALUES (8,'뽕사부','중식','{\"짜장면\" : 9000,\"짬뽕\" : 12000,\"볶음밥\" : 11000,\"탕수육(소)\" : 9900}',4.3,'https://naver.me/Fmgfe9Tu',429,0);
