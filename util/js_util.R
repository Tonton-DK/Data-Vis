js <-
  "

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
const sections = document.querySelectorAll('section[id]');
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
/*    background-color: #f2f2f2; */
    min-height: 100vh;
    margin: 0;
    padding: 2.5rem 4rem;
}
/*
section:nth-of-type(2n) {
    background-color: #ccc;
}

section:last-of-type {
    height: 100vh;
}
*/
.sidenav {
    height: 100%;
    width: 0;
    position: fixed;
    z-index: 1;
    top: 0;
    left: 0;
    width: 250px;
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
    margin-left: 250px;
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