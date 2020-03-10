library(tidyverse)
library(plotly)
library(moderndive)



# Define 3D scatterplot points --------------------------------------------
# Get coordinates of points for 3D scatterplot
x_values <- CASchools$STR %>% 
    round(3)
y_values <- CASchools$english %>% 
    round(3)
z_values <- CASchools$score %>% 
    round(3)


# Define regression plane -------------------------------------------------
# Construct x and y grid elements
x_grid <- seq(from = min(x_values), to = max(x_values), length = 50)
y_grid <- seq(from = min(y_values), to = max(y_values), length = 50)

# Construct z grid by computing
# 1) fitted beta coefficients
# 2) fitted values of outer product of x_grid and y_grid
# 3) extracting z_grid (matrix needs to be of specific dimensions)
beta_hat <- CASchools %>% 
    lm(score ~ STR + english, data = .) %>% 
    coef()
fitted_values <- crossing(y_grid, x_grid) %>% 
    mutate(z_grid = beta_hat[1] + beta_hat[2]*x_grid + beta_hat[3]*y_grid)
z_grid <- fitted_values %>% 
    pull(z_grid) %>%
    matrix(nrow = length(x_grid)) %>%
    t()

# Define text element for each point in plane
text_grid <- fitted_values %>% 
    pull(z_grid) %>%
    round(3) %>% 
    as.character() %>% 
    paste("score", ., sep = "") %>% 
    matrix(nrow = length(x_grid)) %>%
    t()


# Plot using plotly -------------------------------------------------------
fig <- plot_ly() %>%
    # 3D scatterplot:
    add_markers(
        x = x_values,
        y = y_values,
        z = z_values,
        marker = list(size = 5),
        hoverinfo = 'text',
        text = ~paste(
            "score ", z_values, "<br>",
            "english ", y_values, "<br>",
            "STR ", x_values 
        )
    ) %>%
    # Regression plane:
    add_surface(
        x = x_grid,
        y = y_grid,
        z = z_grid,
        hoverinfo = 'text',
        text = text_grid
    ) %>%
    # Axes labels and title:
    layout(
        title = "3D Reg Plane",
        scene = list(
            zaxis = list(title = "y: score"),
            yaxis = list(title = "x2: English"),
            xaxis = list(title = "x1: STR")
        )
    )
htmlwidgets::saveWidget(fig
                        ,paste0("3dreg.html"))

fig2 <- plot_ly(x = kde$x, y = kde$y, z = kde$z) %>% add_surface(
    contours = list(
        z = list(
            show=TRUE,
            usecolormap=TRUE,
            highlightcolor="#ff0000",
            project=list(z=TRUE)
        )
    )
)
fig2 <- fig2 %>% layout(
    scene = list(
        camera=list(
            eye = list(x=1.87, y=0.88, z=-0.64)
        )
    )
)
htmlwidgets::saveWidget(fig2
                       ,paste0("3dreg_joint_dist.html"))

fig3 <- iplotCorr(datacor, reorder = T, 
          chartOpts=list(cortitle="Correlation matrix",
                                               scattitle="Scatterplot"))
htmlwidgets::saveWidget(fig3
                        ,paste0("corr_scat.html"))
