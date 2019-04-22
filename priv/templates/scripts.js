(function () {
    var booksContainer = document.querySelector('.books');
    var chaptersMenu = document.querySelector('.chapters-menu');
    var chaptersContainer = document.querySelector('.chapters');
    var chapter = document.querySelector('.chapter');
    var chapterContainer = document.querySelector('.chapter-container');

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
        chapterContainer.querySelectorAll('.verse').forEach(function (verse) {
            verse.classList.remove('active');
            var a = verse.querySelector('.interlinear-link');
            if (a) {
                verse.removeChild(a);
            }
        });

        verseNode.classList.add('active');
        
        var interlinearLink = document.createElement('a');
        interlinearLink.setAttribute('target', '_blank');
        interlinearLink.className = 'interlinear-link';
        interlinearLink.setAttribute('title', 'Otwórz w Biblii Interlinearnej');
        
        var chapterNumber = chapter.dataset.chapterNumber;
        var verseNumber = verseNode.dataset.verseNumber;

        var ntBookIndex = chapter.dataset.ntIndex;
        var otBookName = chapter.dataset.otName;
        if (ntBookIndex) {
            interlinearLink.setAttribute(
                'href',
                'http://biblia.oblubienica.eu/interlinearny/index'
                    + '/book/' + ntBookIndex
                    + '/chapter/' + chapterNumber
                    + '/verse/' + verseNumber
            );
        } else if (otBookName) {
            interlinearLink.setAttribute(
                'href',
                'https://biblehub.com/'
                    + otBookName
                    + '/' + chapterNumber + '-' + verseNumber + '.htm'
                    + '#combox'
            );
        }

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
        selectVerseByNumber(location.hash.substr(1));
    }

    if (location.search === '?chapters') {
        const { encodedName, chapterNumber } = readLocation();
        const link = booksContainer.querySelector('a[data-encoded-name="' + encodedName + '"]');
        console.log('link', { link, encodedName, chapterNumber });
        showChapter(link.getAttribute('title'), encodedName, parseInt(link.dataset.nofChapters, 10));
    }

    chaptersMenu.querySelector('.back-button').addEventListener('click', function (event) {
        event.preventDefault();
        history.back();
    });

    window.addEventListener('popstate', function (event) {
        var state = event.state;
        if (state && state.selectedVerse) {
            showBooksUI();
            loadChapter(state.encodedName, state.chapterNumber)
                .then(() => selectVerseByNumber(state.selectedVerse, false)); 
        } else if (state && state.bookName) {
            showChapter(state.bookName, state.encodedName, state.nofChapters);
        } else {
            showBooksUI();
            const {encodedName, chapterNumber} = readLocation();
            loadChapter(encodedName, chapterNumber);
            if (location.hash) {
                selectVerseByNumber(location.hash.substr(1), false);
            }
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
        if (chapter.dataset.bookEncodedName === bookEncodedName && chapter.dataset.chapterNumber === chapterNumber) {
            return Promise.resolve();
        }
        return fetch('/' + bookEncodedName + '/' + chapterNumber + '?noLayout=true')
            .then(data => data.text())
            .then(chapter => {
                document.querySelector('.chapter-container').innerHTML = chapter;
                chapter = document.querySelector('.chapter');
            });
    }

    booksContainer.addEventListener('click', function (event) {
        if (event.target.nodeName.toLowerCase() === 'a') {
            event.preventDefault();
            var linkNode = event.target;
            var bookName = linkNode.getAttribute('title') || '';
            var encodedName = linkNode.dataset.encodedName || '';
            var nofChapters = parseInt(linkNode.dataset.nofChapters, 10);

            history.pushState({
                bookName: bookName,
                nofChapters: nofChapters,
                encodedName: encodedName
            }, '', '/' + encodedName + '/1?chapters');

            showChapter(bookName, encodedName, nofChapters);
        }
    }, true);

    chapterContainer.addEventListener('click', function (event) {
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
            history.pushState({
                bookName: chapter.dataset.bookName,
                encodedName: chapter.dataset.encodedBookName,
                chapterNumber: chapter.dataset.chapterNumber,
                selectedVerse: container.dataset.verseNumber
            }, '', '/' + chapter.dataset.encodedBookName + '/' + chapter.dataset.chapterNumber + '#' + container.dataset.verseNumber);
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
    
    function selectVerseByNumber(verseNumber, scroll = true) {
        var foundVerse = chapterContainer.querySelector('[data-verse-number="' + verseNumber + '"]');
        if (foundVerse) {
            if (scroll) {
                window.scrollTo(0, foundVerse.offsetTop);
            }
            selectVerse(foundVerse);
        }
    }

    function readLocation() {
        let [nothing, encodedName, chapterNumber] = location.pathname.split('/');
        encodedName = encodedName || 'rdz';
        chapterNumber = chapterNumber || 1;

        return {encodedName, chapterNumber};
    }
}());
