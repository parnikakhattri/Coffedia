Coffedia - Your Encyclopedia of Coffee ☕

Purpose:
Coffedia is an interactive R Shiny application designed for coffee enthusiasts to explore coffee brands, stores and trends across the world. 
The app allows users to visualise brand popularity, explore coffee outlets and submit feedback on their coffee experiences.

Features:
1. About Tab:
- Provides an overview of Coffedia's mission, features, and the inspiration behind its development.

2. Popularity Tab:
- Visualise the popularity of different coffee brands within a selected country.
- Interactive filters to select countries and brands for dynamic visualisation.

3. Outlets Tab:
- Explore coffee outlets in various cities with an easy-to-use table.
- Filter results by country, city, and coffee brand to find your desired coffee spots.

4. Feedback Tab:
- Submit your feedback on coffee brands and stores, including ratings and reviews.
- Visualise user feedback as a word cloud and detailed individual reviews.

Data Requirements:
- The app uses an Excel file located in the "data/" folder (e.g., `coffee_chains.xlsx`) containing details about coffee outlets.
- Feedback data is stored in `data/feedback_data.csv`.

Setup Instructions:
1. Ensure you have the following libraries installed in R:
shiny, plotly, ggplot2, DT, readxl, bslib, shinycssloaders, tidyverse, shinyWidgets, wordcloud2
You can install these packages using:
install.packages(c("shiny", "plotly", "ggplot2", "DT", "readxl", "bslib", "shinycssloaders", "tidyverse", "shinyWidgets", "wordcloud2"))

2. Place the following files in the respective locations:
- `coffee_chains.xlsx`: Inside the `data/` folder.
- The app script (e.g., `app.R`) in the root project folder.

3. Run the application:
- Open the app script in RStudio and run the following command in the R console:
shiny::runApp()

File Structure:
- app.R: Main R Shiny script for the app.
- data/coffee_chains.xlsx: Contains information about coffee outlets (country, city, brand, etc.).
- data/feedback_data.csv: Stores user feedback submitted through the app.

Usage Instructions:
- Launch the app by running `shiny::runApp()` in RStudio.
- Navigate through the app using the tabs at the top:
- Learn more about Coffedia on the About Tab.
- Explore brand popularity and outlets on their respective tabs.
- Submit and view feedback on the Feedback Tab.

Shiny App Link:
The Coffedia Shiny app is live and can be accessed here: http://parnikakhattri.shinyapps.io/Coffedia1

Contact:
If you encounter any issues or have suggestions for improvements, feel free to contact the developer.

License:
This project is open-source and can be used or modified for personal and educational purposes.
