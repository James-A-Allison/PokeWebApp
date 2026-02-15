import requests
import time
import csv
from bs4 import BeautifulSoup
from urllib.parse import urljoin

BASE_URL = "https://db.pokemongohub.net"
LIST_URL = f"{BASE_URL}/pokemon-list"

HEADERS = {
    "User-Agent": "Mozilla/5.0 (compatible; PokemonMoveScraper/1.0)"
}

def get_soup(url):
    """Fetch and return BeautifulSoup object with retry logic."""
    for attempt in range(3):
        try:
            response = requests.get(url, headers=HEADERS, timeout=10)
            response.raise_for_status()
            return BeautifulSoup(response.text, "html.parser")
        except requests.RequestException:
            print(f"Retrying {url} (attempt {attempt+1})")
            time.sleep(2)
    print(f"Failed to retrieve {url}")
    return None


def extract_pokemon_links():
    """Extract all Pokémon page URLs."""
    soup = get_soup(LIST_URL)
    if not soup:
        return []

    links = set()

    for a in soup.select("a[href^='/pokemon/']"):
        href = a.get("href")
        if href.count("/") == 2:  # avoid sub-pages like /pokemon/1/moves
            full_url = urljoin(BASE_URL, href)
            links.add(full_url)

    print(f"Found {len(links)} Pokémon pages")
    return sorted(links)


def is_legacy(row):
    """Detect if move row indicates legacy."""
    text = row.get_text().lower()

    if "*" in text:
        return True
    if "legacy" in text:
        return True

    # Check CSS classes
    if "legacy" in " ".join(row.get("class", [])).lower():
        return True

    return False


def extract_moves(pokemon_url):
    """Extract moves from a single Pokémon page."""
    soup = get_soup(pokemon_url)
    if not soup:
        return []

    pokemon_name_tag = soup.find("h1")
    if not pokemon_name_tag:
        return []

    pokemon_name = pokemon_name_tag.get_text(strip=True)

    rows = []

    tables = soup.find_all("table")

    for table in tables:
        header = table.find_previous("h2")
        if not header:
            continue

        header_text = header.get_text().lower()

        if "fast" in header_text:
            move_type = "Fast"
        elif "charged" in header_text:
            move_type = "Charged"
        else:
            continue

        for tr in table.find_all("tr")[1:]:  # skip header row
            cols = tr.find_all("td")
            if not cols:
                continue

            move_name = cols[0].get_text(strip=True)

            legacy_flag = is_legacy(tr)

            rows.append({
                "Pokemon": pokemon_name,
                "Move": move_name,
                "Move_Type": move_type,
                "Legacy": legacy_flag
            })

    return rows


def main():
    TOTAL_POKEMON = 1025
    BASE_URL = "https://db.pokemongohub.net/pokemon"

    all_rows = []

    for i in range(1, TOTAL_POKEMON + 1):
        link = f"{BASE_URL}/{i}"

        print(f"[{i}/{TOTAL_POKEMON}] Scraping {link}")

        moves = extract_moves(link)

        # Some IDs may not exist or may redirect
        if moves:
            all_rows.extend(moves)

        time.sleep(1)  # rate limit

    print(f"Collected {len(all_rows)} move records")


if __name__ == "__main__":
    main()