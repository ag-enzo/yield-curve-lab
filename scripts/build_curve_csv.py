#!/usr/bin/env python3
"""Merge individual tenor CSV exports into the tidy curve CSV.

Usage:
    python build_curve_csv.py --date 2025-10-02 --output data/ecb_sample_2025-10-02.csv

The script expects per-tenor CSV files in `data/` named like `ecb-3m.csv`,
`ecb-6m.csv`, `ecb-1y.csv`, etc., each with the ECB Data Portal export format.
"""
from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path
from typing import Iterable, List, Tuple

BASE_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = BASE_DIR / "data"

TENOR_FILE_MAP: List[Tuple[str, Path]] = [
    ("0.25", DATA_DIR / "ecb-3m.csv"),
    ("0.50", DATA_DIR / "ecb-6m.csv"),
    ("1.00", DATA_DIR / "ecb-1y.csv"),
    ("2.00", DATA_DIR / "ecb-2y.csv"),
    ("5.00", DATA_DIR / "ecb-5y.csv"),
    ("10.00", DATA_DIR / "ecb-10y.csv"),
    ("30.00", DATA_DIR / "ecb-30y.csv"),
]


def parse_args(argv: Iterable[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build tidy curve CSV from per-tenor exports.")
    parser.add_argument("--date", required=True, help="Curve date (YYYY-MM-DD)")
    parser.add_argument("--output", required=True, help="Output CSV path")
    return parser.parse_args(argv)


def read_rate(path: Path, target_date: str) -> str:
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.reader(fh)
        header = next(reader, None)
        if header is None:
            raise ValueError(f"File {path} is empty")
        for row in reader:
            if not row:
                continue
            if row[0].strip('"') == target_date:
                return row[2].strip('"')
    raise ValueError(f"Date {target_date} not found in {path}")


def write_curve(date: str, output_path: Path) -> None:
    rows = []
    for tenor, path in TENOR_FILE_MAP:
        if not path.exists():
            raise FileNotFoundError(f"Missing tenor file: {path}")
        rate = read_rate(path, date)
        rows.append((date, tenor, rate))
    rows.sort(key=lambda item: float(item[1]))
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(["date", "tenor_years", "zero_rate_cc"])
        for row in rows:
            writer.writerow(row)


def main(argv: Iterable[str]) -> int:
    args = parse_args(argv)
    output_path = Path(args.output)
    try:
        write_curve(args.date, output_path)
    except Exception as exc:  # pragma: no cover - convenience tool
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    print(f"Wrote {output_path} for {args.date}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
