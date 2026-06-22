#!/usr/bin/env python3
"""Gera data/dc_catalog.tsv (nome normalizado -> gênero|ano|developer) a partir
do Wikidata, para o enriquecimento de catálogo da biblioteca.

Uso:
    python3 scripts/gen_dc_catalog.py

Requer apenas `curl` no PATH. Sem API key. Reexecute para atualizar o catálogo.

Colunas do TSV: normname  displayname  genero  ano  developer
- normname: minúsculas, só [a-z0-9] — MESMA normalização do Pascal
  (NormalizeForMatch após CleanGameNameForSearch), para casar por nome.
- genero: uma de ~12 categorias PT (mapeadas dos gêneros granulares do Wikidata).
"""
import re, csv, subprocess, sys

SPARQL = """
SELECT ?gameLabel ?genreLabel ?year ?devLabel WHERE {
  ?game wdt:P400 wd:Q184198 .                 # plataforma = Dreamcast
  OPTIONAL { ?game wdt:P136 ?genre . }        # gênero
  OPTIONAL { ?game wdt:P577 ?date . BIND(YEAR(?date) AS ?year) }  # lançamento
  OPTIONAL { ?game wdt:P178 ?dev . }          # desenvolvedora
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
"""

# substring do gênero (Wikidata, lower) -> categoria PT
GENRE_MAP = [
    ('fighting', 'Luta'), ("beat 'em up", 'Luta'), ('versus', 'Luta'),
    ('racing', 'Corrida'), ('kart', 'Corrida'),
    ('first-person shooter', 'Tiro'), ('third-person shooter', 'Tiro'),
    ("shoot 'em up", 'Tiro'), ('light gun', 'Tiro'), ('rail shooter', 'Tiro'),
    ('shooter', 'Tiro'),
    ('platform', 'Plataforma'),
    ('role-playing', 'RPG'), ('roguelike', 'RPG'),
    ('rhythm', 'Ritmo'), ('music', 'Ritmo'),
    ('sports', 'Esporte'), ('fishing', 'Esporte'), ('fitness', 'Esporte'),
    ('puzzle', 'Puzzle'),
    ('strategy', 'Estrategia'), ('tactical', 'Estrategia'), ('real-time tactics', 'Estrategia'),
    ('survival horror', 'Aventura'), ('action-adventure', 'Aventura'), ('adventure', 'Aventura'),
    ('visual novel', 'Aventura'), ('interactive fiction', 'Aventura'), ('graphic adventure', 'Aventura'),
    ('dating sim', 'Simulacao'), ('simulation', 'Simulacao'), ('simulator', 'Simulacao'),
    ('hack and slash', 'Acao'), ('action game', 'Acao'), ('arcade', 'Acao'),
]
# Ao escolher entre vários gêneros, o mais específico ganha do mais genérico.
PRIORITY = ['Luta', 'Corrida', 'Tiro', 'Plataforma', 'RPG', 'Ritmo', 'Esporte',
            'Puzzle', 'Estrategia', 'Aventura', 'Simulacao', 'Acao']


def clean(s):
    return s.replace('@en', '').strip().strip('"')


def is_qid(s):
    return bool(re.fullmatch(r'Q\d+', s))


def category(genre):
    g = genre.lower()
    for sub, cat in GENRE_MAP:
        if sub in g:
            return cat
    return None


def norm(name):
    return re.sub('[^a-z0-9]', '', name.lower())


def fetch():
    out = subprocess.run(
        ['curl', '-s', '-G', 'https://query.wikidata.org/sparql',
         '--data-urlencode', 'query=' + SPARQL,
         '-H', 'Accept: text/tab-separated-values',
         '-H', 'User-Agent: GDEMUGUI/1.0 (catalog generator)'],
        capture_output=True, text=True, check=True)
    return out.stdout.splitlines()[1:]  # pula o cabeçalho


def main():
    games = {}
    for line in fetch():
        r = line.split('\t')
        name = clean(r[0]) if r else ''
        if not name or is_qid(name):
            continue
        genre = clean(r[1]) if len(r) > 1 else ''
        year = r[2].strip() if len(r) > 2 else ''
        dev = clean(r[3]) if len(r) > 3 else ''
        g = games.setdefault(name, {'cats': set(), 'years': set(), 'devs': []})
        c = category(genre)
        if c:
            g['cats'].add(c)
        if year.isdigit():
            g['years'].add(int(year))
        if dev and not is_qid(dev) and dev not in g['devs']:
            g['devs'].append(dev)

    rows = []
    for name, g in games.items():
        cat = next((p for p in PRIORITY if p in g['cats']), '')
        year = str(min(g['years'])) if g['years'] else ''
        dev = g['devs'][0] if g['devs'] else ''
        if norm(name):
            rows.append((norm(name), name, cat, year, dev))
    rows.sort(key=lambda x: x[1])

    with open('data/dc_catalog.tsv', 'w', newline='') as f:
        csv.writer(f, delimiter='\t').writerows(rows)

    print(f'{len(rows)} jogos | genero {sum(1 for r in rows if r[2])} | '
          f'ano {sum(1 for r in rows if r[3])} | dev {sum(1 for r in rows if r[4])}')


if __name__ == '__main__':
    sys.exit(main())
