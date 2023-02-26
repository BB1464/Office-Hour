blk <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3)
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)


# Convert block and treatment to factors
blk <- as.factor(blk)
trt <- as.factor(trt)


## Clinometer
https://www.youtube.com/watch?v=ZQDEU2R7aI4

https://www.youtube.com/watch?v=X6QaHaoInh4

https://www.youtube.com/watch?v=Vf7eqQu5GY0

https://www.youtube.com/watch?v=rVNhDZOwVU8

https://www.youtube.com/watch?v=zjuiKhbZPMs




out1 <- augmentedRCBD(blk, trt, y1, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE)

