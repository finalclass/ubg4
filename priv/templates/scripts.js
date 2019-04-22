(function () {
    var booksContainer = document.querySelector('.books');
    var chaptersMenu = document.querySelector('.chapters-menu');
    var chaptersContainer = document.querySelector('.chapters');
    var versesContainer = document.querySelector('.content');

    function showBooksUI() {
        chaptersMenu.style.display = 'none';
        chaptersContainer.style.display = 'none';
        booksContainer.style.display = 'flex';
    }

    function showChaptersUI() {
        chaptersContainer.style.display = 'block';
        chaptersMenu.style.display = 'flex';
        booksContainer.style.display = 'none';
    }

    function selectVerse(verseNode) {
        versesContainer.querySelectorAll('.verse').forEach(function (verse) {
            verse.classList.remove('active');
            var a = verse.querySelector('.interlinear-link');
            if (a) {
                verse.removeChild(a);
            }
        });

        verseNode.classList.add('active');
        
        var bookNumber = versesContainer.dataset.ntIndex;
        if (!bookNumber) {
            // don't add interlinear link if on old testament (not available yet)
            return;
        }
        var interlinearLink = document.createElement('a');
        interlinearLink.setAttribute('target', '_blank');
        interlinearLink.className = 'interlinear-link';
        interlinearLink.setAttribute('title', 'Otwórz w Biblii Interlinearnej');
        
        var chapterNumber = versesContainer.dataset.chapterNumber;
        var verseNumber = verseNode.dataset.verseNumber;
        
        interlinearLink.setAttribute(
            'href',
            'http://biblia.oblubienica.eu/interlinearny/index'
                + '/book/' + bookNumber
                + '/chapter/' + chapterNumber
                + '/verse/' + verseNumber
        );
        verseNode.appendChild(interlinearLink);
    }

    var flashMessageContainer = document.querySelector('.flash-message span');
    var hideFlashMessageTimeout;
    function showFlashMessage(msg) {
        clearTimeout(hideFlashMessageTimeout);
        flashMessageContainer.textContent = msg;
        flashMessageContainer.style.display = 'inline';
        hideFlashMessageTimeout = setTimeout(function () {
            flashMessageContainer.textContent = '';
            flashMessageContainer.style.display = 'none';
        }, 3000);
    }

    showBooksUI();

    if (location.hash) {
        var foundVerse = versesContainer.querySelector('[data-verse-number="' + location.hash.substr(1) + '"]');
        if (foundVerse) {
            window.scrollTo(0, foundVerse.offsetTop);
            selectVerse(foundVerse);
        }
    }

    chaptersMenu.querySelector('.back-button').addEventListener('click', function (event) {
        event.preventDefault();
        history.back();
    });

    window.addEventListener('popstate', function (event) {
        var state = event.state;

        if (state && state.bookName) {
            showChapter(state.bookName, state.encodedName, state.nofChapters);
        } else {
            showBooksUI();
            const [nothing, encodedName, chapterNumber] = location.pathname.split('/');
            loadChapter(encodedName, chapterNumber);
        }
    });

    function showChapter(bookName, encodedName, nofChapters) {
        chaptersMenu.querySelector('.book-name').innerText = bookName;

        while (chaptersContainer.hasChildNodes()) {
            chaptersContainer.removeChild(chaptersContainer.lastChild);
        }

        for (var i = 1; i < nofChapters + 1; i += 1) {
            var li = document.createElement('li');
            var a = document.createElement('a');
            a.setAttribute('href', '/' + encodedName + '/' + i);
            a.innerText = i;
            li.appendChild(a);
            chaptersContainer.appendChild(li);
        }

        showChaptersUI();
        loadChapter(encodedName, 1);
    }

    function loadChapter(bookEncodedName = 'rdz', chapterNumber = 1) {
        return fetch('/' + bookEncodedName + '/' + chapterNumber + '?noLayout=true')
            .then(data => data.text())
            .then(chapter => {
                document.querySelector('.chapter-container').innerHTML = chapter;
                versesContainer = document.querySelector('.content');
            });
    }

    booksContainer.addEventListener('click', function (event) {
        if (event.target.nodeName.toLowerCase() === 'a') {
            event.preventDefault();
            var linkNode = event.target;
            var bookName = linkNode.getAttribute('title') || '';
            var encodedName = linkNode.getAttribute('encoded-name') || '';
            var nofChapters = parseInt(linkNode.getAttribute('nof-chapters'), 10);

            history.pushState({
                bookName: bookName,
                nofChapters: nofChapters,
                encodedName: encodedName
            }, '', '/' + encodedName + '/1');

            showChapter(bookName, encodedName, nofChapters);
        }
    }, true);

    versesContainer.addEventListener('click', function (event) {
        var hasVerseClass = event.target.classList.contains('verse');
        var hasVerseTextClass = event.target.classList.contains('verse-text');
        
        if (hasVerseClass || hasVerseTextClass) {
            var container = hasVerseTextClass ? event.target.parentNode : event.target;
            
            if (Date.now() - (container.__lastClick || 0) < 500) {
                var verseText = container.querySelector('.verse-text').textContent.trim();
                copyToClipboard(verseText);
                return showFlashMessage('Skopiowano');
            }

            container.__lastClick = Date.now();
            event.preventDefault();
            location.hash = container.dataset.verseNumber;
            selectVerse(container);
        }
    }, true);

    function copyToClipboard(text) {
        var textArea = document.createElement('textarea');
        textArea.value = text;
        // without this the page scrolls down when we copy:
        textArea.style.position = 'fixed';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
    }
}());
