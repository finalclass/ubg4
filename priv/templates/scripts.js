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

    chaptersMenu.querySelector('.back-button').addEventListener('click', function (event) {
        event.preventDefault();
        history.back();
    });

    function initFromLocation() {
        var loc = readLocation();

        if (loc.chapters) {
            showChaptersMenu(loc.encodedName);
        } else {
            showBooksUI();
        }
        
        loadChapter(loc.encodedName, loc.chapterNumber, function () {
            if (loc.verse) {
                selectVerseByNumber(loc.verse, false);
            }
        });
    }
    
    window.addEventListener('popstate', initFromLocation);

    initFromLocation();

    function showChaptersMenu(encodedName) {
        var link = booksContainer.querySelector('a[data-encoded-name="' + encodedName + '"]');
        var bookName = link.getAttribute('title') || '';
        var nofChapters = parseInt(link.dataset.nofChapters, 10);
        
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
    }

    function loadChapter(bookEncodedName, chapterNumber, callback) {
        var currentEncodedBookName = chapter.dataset.encodedBookName;
        var currentChapterNumber = parseInt(chapter.dataset.chapterNumber, 10);
        if (currentEncodedBookName === bookEncodedName && currentChapterNumber === chapterNumber) {
            if (callback) {
                callback();
            }
            return;
        }
        var req = new XMLHttpRequest();
        req.addEventListener('load', function () {
            var chapter = req.responseText;
            document.querySelector('.chapter-container').innerHTML = chapter;
            chapter = document.querySelector('.chapter');
            if (callback) {
                callback();
            }
        });
        req.open('GET', '/' + bookEncodedName + '/' + chapterNumber + '?noLayout=true');
        req.send();
    }

    booksContainer.addEventListener('click', function (event) {
        if (event.target.nodeName.toLowerCase() === 'a') {
            event.preventDefault();
            var linkNode = event.target;
            var bookName = linkNode.getAttribute('title') || '';
            var encodedName = linkNode.dataset.encodedName || '';
            var nofChapters = parseInt(linkNode.dataset.nofChapters, 10);

            history.pushState({}, '', '/' + encodedName + '/1?chapters');

            showChaptersMenu(encodedName);
            loadChapter(encodedName, 1);
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
            var link = location.pathname + location.search + '#' + container.dataset.verseNumber;
            history.pushState({}, '', link);
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
        var split = location.pathname.split('/');
        var encodedName = split[1];
        var chapterNumber = split[2];

        return {
            encodedName: encodedName || 'rdz',
            chapterNumber: parseInt(chapterNumber, 10) || 1,
            chapters: location.search === '?chapters',
            verse: parseInt((location.hash || '').substr(1), 10)
        };
    }
}());
