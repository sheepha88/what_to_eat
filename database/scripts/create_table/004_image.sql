use what_to_eatDB ;

CREATE TABLE `image` (
  `id` int NOT NULL AUTO_INCREMENT,
  `path` text COLLATE utf8mb4_unicode_ci NOT NULL,
  `size` varchar(45) COLLATE utf8mb4_unicode_ci NOT NULL,
  `res_id` int NOT NULL,
  `user_id` int NOT NULL,
  `review_id` int NOT NULL,
  PRIMARY KEY (`id`),
  KEY `res_id` (`res_id`),
  KEY `user_id` (`user_id`),
  KEY `review_id` (`review_id`),
  CONSTRAINT `image_ibfk_1` FOREIGN KEY (`res_id`) REFERENCES `res` (`id`),
  CONSTRAINT `image_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`),
  CONSTRAINT `image_ibfk_3` FOREIGN KEY (`review_id`) REFERENCES `review` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='이미지'