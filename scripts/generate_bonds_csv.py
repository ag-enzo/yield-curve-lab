#!/usr/bin/env python3
"""
Generate the MVP bonds CSV for YieldCurveLab.

Usage example (from project root):
    cd scripts
    uv run generate_bonds_csv.py \
        --settle 2025-10-02 \
        --output ../input/bonds.csv
"""
from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Iterable, List, Tuple
import sys

BASE_DIR = Path(__file__).resolve().parents[1]

def parse_args(argv: Iterable[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Write the sample bonds.csv file.")
    parser.add_argument(
        "--settle",
        default="2025-10-02",
        help="Settlement date (YYYY-MM-DD). Used for coupon schedule start.",
    )
    parser.add_argument(
        "--output",
        default="input/bonds.csv",
        help="Output CSV path relative to project root.",
    )
    return parser.parse_args(argv)


def build_rows(settle_date: str) -> List[Tuple[str, str, str, str, str, str]]:
    return [
        (
            "DEMO-5Y",
            settle_date,
            add_years(settle_date, 5),
            "0.025",
            "1000000",
            "1",
        ),
        (
            "DEMO-10Y",
            settle_date,
            add_years(settle_date, 10),
            "0.035",
            "1000000",
            "1",
        ),
        (
            "DEMO-0C-3Y",
            settle_date,
            add_years(settle_date, 3),
            "0.000",
            "1000000",
            "1",
        ),
    ]


def add_years(date_str: str, years: int) -> str:
    """Add whole years to a YYYY-MM-DD string (naive; good enough for demo data)."""
    year, month, day = map(int, date_str.split("-"))
    return f"{year + years:04d}-{month:02d}-{day:02d}"


def write_csv(path: Path, rows: List[Tuple[str, str, str, str, str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["id", "settle_date", "maturity", "coupon_rate", "notional", "frequency"])
        writer.writerows(rows)


def main(argv: Iterable[str]) -> int:
    args = parse_args(argv)
    raw_output = Path(args.output)
    output_path = raw_output if raw_output.is_absolute() else (BASE_DIR / raw_output)
    rows = build_rows(args.settle)
    write_csv(output_path, rows)
    print(f"Wrote {output_path} with {len(rows)} sample bonds.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
