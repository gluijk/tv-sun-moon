# Relative observed size of the Sun and the Moon vs a regular TV screen
# www.overfitting.net
# https://www.overfitting.net/2026/06/el-sol-y-la-luna-se-ven-mas-pequenos-de.html

library(Cairo)

dibujar_comparacion_visual <- function(pulgadas, distancia_m, aumento = 12) {
    
    # 1. Dimensiones de la TV (16:9)
    # Teorema de Pitágoras para sacar ancho y alto (16^2 + 9^2 = 337)
    pulgadas_a_m <- 0.0254
    diag_m <- pulgadas * pulgadas_a_m
    ancho_tv <- diag_m * (16 / sqrt(337))
    alto_tv  <- diag_m * (9  / sqrt(337))
    
    # 2. Ángulos aparentes
    # El diámetro angular promedio del Sol es ~0.533 grados y el de la Luna ~0.518 grados
    angulo_sol_rad  <- 0.533 * pi / 180
    angulo_luna_rad <- 0.518 * pi / 180
    
    # Fórmula geométrica: Tamaño = 2 * distancia * tan(ángulo / 2)
    diam_sol_m  <- 2 * distancia_m * tan(angulo_sol_rad/2)
    diam_luna_m <- 2 * distancia_m * tan(angulo_luna_rad/2)
    
    # Tamaño aparente con binoculares
    
    # Aproximación (válida en un 99.99%)
    # diam_sol_bin_m  <- diam_sol_m  * aumento
    # diam_luna_bin_m <- diam_luna_m * aumento
    
    # Valor exacto (el aumento 12x es angular)
    angulo_sol_bin_rad  <- aumento * angulo_sol_rad
    angulo_luna_bin_rad <- aumento * angulo_luna_rad
    diam_sol_bin_m  <- 2 * distancia_m * tan(angulo_sol_bin_rad/2)
    diam_luna_bin_m <- 2 * distancia_m * tan(angulo_luna_bin_rad/2)

    
    # 3. Configuración del gráfico
    # asp=1 es crucial para que los metros en X e Y midan lo mismo visualmente y no se deforme
    margen <- max(diam_sol_bin_m, diam_luna_bin_m) * 0.7

    plot(
        0, 0, type = "n",
        xlim = c(-ancho_tv/2 * 1.1, ancho_tv/2 * 1.1),
        ylim = c(-alto_tv/2 * 1.1, alto_tv/2 * 1.1),
        asp = 1,
        xlab = "Ancho (m)",
        ylab = "Alto (m)",
        main = paste0("TV de ", pulgadas, "\" vista a ", distancia_m, "m"),
        cex.main = 1.8,
        cex.lab = 1.5,
        cex.axis = 1.5
    )
    
    # Dibujar el marco de la TV
    rect(-ancho_tv/2, -alto_tv/2, ancho_tv/2, alto_tv/2, col = "black")
    
    
    # Función auxiliar para dibujar círculos (polígonos de muchos lados)
    dibujar_circulo <- function(x, y, diametro, col) {
        radio <- diametro / 2
        theta <- seq(0, 2*pi, length.out = 800)
        polygon(
            x + radio*cos(theta),
            y + radio*sin(theta),
            col = col,
            border = NA
        )
    }
    
    # 4. Dibujar el Sol y la Luna dentro de la TV
    # Los separamos un poco para que se comparen bien uno al lado del otro
    separacion_normal <- diam_sol_m * 5
    separacion_bin    <- diam_sol_bin_m * 1.4
    y_normal <-  alto_tv/3
    y_bin    <- -alto_tv/8
    
    # Dibujar tamaño real
    dibujar_circulo(-separacion_normal/2, y_normal, diam_sol_m, "#FFD700")
    dibujar_circulo(separacion_normal/2, y_normal, diam_luna_m, "#E0E0E0")

    # Dibujar tamaño con binoculares
    dibujar_circulo(-separacion_bin/2, y_bin, diam_sol_bin_m, "#FFD700")
    dibujar_circulo(separacion_bin/2, y_bin, diam_luna_bin_m, "#E0E0E0")
    
    # Añadir etiquetas
    text(-separacion_normal/2, y_normal - diam_sol_m, "Sol\n(0.53°)",
         pos = 1, cex = 1.6, font = 2, col = "darkgray")
    text(separacion_normal/2, y_normal - diam_luna_m, "Luna\n(0.52°)",
         pos = 1, cex = 1.6, font = 2, col = "darkgray")
    text(-separacion_bin/2, y_bin, paste0("Sol\n", aumento, "×"),
         cex = 2, font = 2, col = "black")
    text(separacion_bin/2, y_bin, paste0("Luna\n", aumento, "×"),
         cex = 2, font = 2, col = "black")
    
    # Mensaje en la consola con los datos exactos
    cat("\n-----------------------------------------\n")
    cat(sprintf("TV de %.1f pulgadas\n", pulgadas))
    cat(sprintf("Ancho: %.3f m\n", ancho_tv))
    cat(sprintf("Alto : %.3f m\n", alto_tv))
    
    cat("\nVisión normal\n")
    cat(sprintf("Sol : %.2f cm\n", diam_sol_m*100))
    cat(sprintf("Luna: %.2f cm\n", diam_luna_m*100))
    
    cat(sprintf("\nCon binoculares %d×\n", aumento))
    cat(sprintf("Sol : %.2f cm\n", diam_sol_bin_m*100))
    cat(sprintf("Luna: %.2f cm\n", diam_luna_bin_m*100))
}

#####################################


# Ejemplos de TV a una distancia de 3m
DIMX <- 1024  # DIMX <- 512
DIMY <- DIMX*9/16

CairoPNG("tv_sun_moon_42_3m.png", width = DIMX, height = DIMY)
dibujar_comparacion_visual(pulgadas = 42, distancia_m = 3)
dev.off()

CairoPNG("tv_sun_moon_55_3m.png", width = DIMX, height = DIMY)
dibujar_comparacion_visual(pulgadas = 55, distancia_m = 3)
dev.off()

CairoPNG("tv_sun_moon_65_3m.png", width = DIMX, height = DIMY)
dibujar_comparacion_visual(pulgadas = 65, distancia_m = 3)
dev.off()
