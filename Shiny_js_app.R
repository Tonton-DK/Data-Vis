library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
source('ingestion/data.R')
source('ingestion/style.R')

source('processing/country.R')
source('processing/region.R')
source('processing/sector.R')
source('processing/emission.R')

source('report/intro.R')


# Ensures data isn't reloaded constantly
load_q6_data()
load_q5_data()
load_q3_data()
load_q2_data()
load_q4_data()

js <-
"
const sections = document.querySelectorAll('section[id]');
var open = false;

function openNav() {
    if (!open) {
        document.getElementById('sidenav').style.width = '250px';
        document.getElementById('main').style.marginLeft = '250px';
        open = !open;
    } else {
        closeNav();
    }
}

function closeNav() {
    document.getElementById('sidenav').style.width = '0';
    document.getElementById('main').style.marginLeft = '0';
    open = !open;
}


document.addEventListener('scroll', function (event) {

    let scroll = window.pageYOffset;
    sections.forEach(function (current) {
        const sectionHeight = current.offsetHeight;
        const sectionTop = current.offsetTop - 50;
        sectionId = current.getAttribute('id');
        if (
            scroll > sectionTop &&
            scroll <= sectionTop + sectionHeight
        ) {
            document.querySelector('a[href*=' + sectionId + ']').classList.add('active');
        } else {
            document.querySelector('a[href*=' + sectionId + ']').classList.remove('active');
        }
    });
});
"

css <-
"
body {
    font-family: 'Lato', sans-serif;
}

section {
    background-color: #f2f2f2;
    height: 90vh;
    margin: 0;
    padding: 2.5rem 4rem;
}

section:nth-of-type(2n) {
    background-color: #ccc;
}

section:last-of-type {
    height: 100vh;
}

.sidenav {
    height: 100%;
    width: 0;
    position: fixed;
    z-index: 1;
    top: 0;
    left: 0;
    background-color: #111;
    overflow-x: hidden;
    transition: 0.5s;
    padding-top: 60px;
}

.sidenav a {
    padding: 8px 8px 8px 32px;
    text-decoration: none;
    font-size: 25px;
    color: #818181;
    display: block;
    transition: 0.3s;
}

.sidenav a:hover {
    color: #f1f1f1;
}

.active {
    color: #f1f1f1 !important;
}

.sidenav .closebtn {
    position: absolute;
    top: 0;
    right: 25px;
    font-size: 36px;
    margin-left: 50px;
}

#main {
    transition: margin-left 0.5s;
    padding: 16px;
}

@media screen and (max-height: 450px) {
    .sidenav {
        padding-top: 15px;
    }

    .sidenav a {
        font-size: 18px;
    }
}
"
ui = fluidPage(
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML(js))
  ),
  tags$div(id = "sidenav", class="sidenav",
    tags$a(href="javascript:void(0)", class="closebtn", onclick="closeNav()", "&times;"),
    tags$a(href="#chart1", "Chart One"),
    tags$a(href="#chart2", "Chart Two"),
    tags$a(href="#chart3", "Chart Three"),
    tags$a(href="#chart4", "Chart Four"),
    tags$a(href="#chart5", "Chart Five")
  ),
  tags$div(id = "main",
    tags$span(style="font-size:30px;cursor:pointer", onclick="openNav()", "&#9776; Sidenav"),
    tags$section(id="chart1",
      tags$h1("Chart 1"),
      intro_ui,
    ),
    tags$section(id="chart2",
      tags$h1("Chart 2"),
      country_ui
    ),
    tags$section(id="chart3",
      tags$h1("Chart 3"),
      region_ui
    ),
    tags$section(id="chart4",
      tags$h1("Chart 4"),
      sector_ui
    ),
    tags$section(id="chart5",
      tags$h1("Chart 5"),
      emission_ui
    )
  )
)

server <- function(input, output) {
  q1_server(input, output)
  q2_server(input, output)
  q3_server(input, output)
  q4_server(input, output)
  q5_server(input, output)
  q6_server(input, output)
  intro_server(input, output)
}

shinyApp(ui, server)
