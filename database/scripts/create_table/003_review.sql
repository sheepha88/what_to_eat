use what_to_eatDB ;

CREATE TABLE `review` (
  `id` int NOT NULL AUTO_INCREMENT,
  `date_visit` date NOT NULL,
  `participants` longtext COLLATE utf8mb4_unicode_ci,
  `rating_TI` float DEFAULT NULL,
  `comment` longtext COLLATE utf8mb4_unicode_ci,
  `res_id` int NOT NULL,
  `user_id` int NOT NULL,
  PRIMARY KEY (`id`),
  KEY `res_id` (`res_id`),
  KEY `user_id` (`user_id`),
  CONSTRAINT `review_ibfk_1` FOREIGN KEY (`res_id`) REFERENCES `res` (`id`),
  CONSTRAINT `review_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='리뷰'
