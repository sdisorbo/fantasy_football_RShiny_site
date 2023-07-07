# fantasy_football_RShiny_site
RShiny website code for a fantasy WAR display--this is a simple use case of RShiny, but acts as a strong template for larger projects

Server file: The server file contains code for gathering NFL fantasy football data and calculating player WAR for each position. It also includes
the name inputs/declarations for the reactable table that will be displayed in the UI

UI file: The UI file contains the code for the reactable table and formats it inside the Shiny site so it will display upon runtime on the site. 
Since the server file collects data in real-time, depending on your computer speed and Shiny website plan, the loading process could take some time
or crash due to insufficient memory. If you don't want to pay for more memory to actually publish the site, keep in mind it always works locally (in this specific use case). 
