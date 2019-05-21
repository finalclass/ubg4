(function () {
    var booksContainer = document.querySelector('.books');
    var chaptersMenu = document.querySelector('.chapters-menu');
    var chaptersContainer = document.querySelector('.chapters');
    var chapterContainer = document.querySelector('.chapter-container');
    var projId;

    if (location.search.indexOf('?proj_id') !== -1) {
        projId = location.search.split('=')[1];
    }
    
    window.addEventListener('popstate', function () {
        initSelectedVerse();
    });

    initSelectedVerse(true);

    function initSelectedVerse(scroll) {
        deselectVerse();
        var verse = parseInt((location.hash || '').substr(1), 10);
        selectVerseByNumber(verse, scroll);
    }

    function getCurrentChapter() {
        return document.querySelector('.chapter');
    }

    function selectVerse (verseNode) {
        deselectVerse();

        verseNode.classList.add('active');

        var interlinearLink = document.createElement('a');
        interlinearLink.setAttribute('target', '_blank');
        interlinearLink.className = 'interlinear-link';
        interlinearLink.setAttribute('title', 'Otwórz w Biblii Interlinearnej');

        var chapter = getCurrentChapter();
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

        document.title = chapter.dataset.bookShortName + ' ' + chapter.dataset.chapterNumber + ':' + verseNumber;
        verseNode.appendChild(interlinearLink);

        if (projId) {
            fetch('/projector', {
                method: 'POST',
                headers: { 'content-type': 'text/plain' },
                body: projId + ';'
                    + chapter.dataset.encodedBookName + ' '
                    + chapter.dataset.chapterNumber + ':'
                    + verseNumber  
            });
        }
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
            location.hash = container.dataset.verseNumber;
            event.preventDefault();
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

    function deselectVerse() {
        chapterContainer.querySelectorAll('.verse').forEach(function (verse) {
            verse.classList.remove('active');
            var a = verse.querySelector('.interlinear-link');
            if (a) {
                verse.removeChild(a);
            }
        });
        var chapter = getCurrentChapter();
        document.title = chapter.dataset.bookShortName + ' ' + chapter.dataset.chapterNumber;
    }

    function selectVerseByNumber(verseNumber, scroll) {
        var foundVerse = chapterContainer.querySelector('[data-verse-number="' + verseNumber + '"]');
        if (foundVerse) {
            selectVerse(foundVerse);
            if (scroll) {
                window.scrollTo(0, foundVerse.offsetTop);
            }
        }
    }
}());
