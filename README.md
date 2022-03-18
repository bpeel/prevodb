PReVo
=====

PReVo estas portebla versio de la Reta Vortaro. Ĝi estas kaj aplikaĵo
por Androjdaj telefonoj, kaj programo por la terminalo en Linukso. Ĉi
tiu deponejo estas la Linuksa parto kaj la ilo por krei la datumbazon
por ambaŭ partoj. La Androjda programo troviĝas ĉi tie:

 http://www.busydoingnothing.co.uk/prevo/

La Reta Vortaro mem troviĝas ĉi tie:

 http://purl.org/net/voko/revo

Kiel kompili la programon
-------------------------

Unue oni devas kloni la deponejon jene:

    git clone https://github.com/bpeel/prevodb.git

PReVo uzas autoconf same kiel multaj aliaj programoj. Tial oni povas
simple kompili ĝin per la jenaj komandoj. Verŝajne vi volos instali
ĝin al loko en via hejmdosiero, kaj tial vi povos uzi la agordilon
`--prefix`:

    ./autogen.sh --prefix=$HOME
    make -j
    make install

Nun la programo estas instalita ĉe `$HOME/bin`. Kutime tio estas en la
serĉvojo por programoj en normalaj instaloj de Linukso do vi povas
simple tajpi ‘prevo’ por startigi ĝin. Tamen unue oni bezonas la
datumbazon. La pakaĵo inkluzivas duan programon por munti ĝin. Tio
bezonas la fontodosierojn de la Reta Vortaro. Vi povas trovi tiujn en
la jena deponejo:

 https://github.com/revuloj/revo-fonto

Vi ankaŭ bezonas la deponejon “voko-grundo” kiu enhavas kelkajn bazajn
dosierojn por agordi la datumbazon. Vi povas kopii ambaŭ deponejojn
per la jenaj komandoj:

    git clone https://github.com/revuloj/revo-fonto.git
    git clone https://github.com/revuloj/voko-grundo.git

Nun vi povas krei la datumbazon per la jenaj komandoj:

    mkdir -p ~/.local/share/prevo
    prevodb -s -i revo-fonto -i voko-grundo -o ~/.local/share/prevo/prevo.db

Tio postulas iom da tempo kaj ĝi verŝajne plendos pri kelkaj eraroj.
Tiuj kutime estas ignoreblaj.

Nun vi povas serĉi en la vortaro tajpante `prevo <vorto>`. Ekzemple:

    prevo terpomo

Se vi volas serĉi en alia lingvo, vi povas unue tajpi la kodon por tiu
lingvo. Ekzemple:

    prevo en potato

Kompletigo en Bash
------------------

PReVo ankaŭ subtenas kompletigon en Bash. Ekzemple vi povas tajpi nur
`prevo terp` kaj premi ‘tab’ dufoje kaj ĝi montros la liston de
kompletigoj:

    $ prevo terp<TAB><TAB>
    terparto      terpeno       terpomfingro  terpomujo
    terpeco       terpinto      terpomfloko   terpoto
    terpeĉo       terpiro       terpomo

Por ebligi tion, oni devas unue inkluzivi unu dosieron en Bash. Tio
eblas per:

    source src/prevo-completion

Por ne devi tajpi tion ĉiufoje, vi povas kopii tiun skripton al
malloka dosierujo ĉi tiel:

    sudo cp src/prevo-completion /etc/bash_completion.d/

Tiel ĝi automate inkluzivos la kodon kiam oni ekkompletigas ion kio
komenciĝas per `prevo`.
