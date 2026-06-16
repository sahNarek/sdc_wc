# TODO: Dockerize `sdc_wc`

Goal: run the full pipeline (build → visualizations → HTML/PDF report) on any machine with a single `docker compose up`, including bundled credentials and match data mounts.

---

## Checklist

### 1. Base image & system dependencies

- [ ] Create `Dockerfile` based on `rocker/r-ver:4.4` (or `rocker/rstudio` for interactive use).
- [ ] Install OS packages required by R visualizations:
  - [ ] `libmagick++-dev`, `imagemagick` — `magick` package (shot-map icon colouring)
  - [ ] `librsvg2-dev` — `rsvg` package (SVG → PNG icons)
  - [ ] `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev` — CRAN installs
  - [ ] `pandoc` — bundled in `rmarkdown` image variants; verify version
  - [ ] `fonts-liberation` or Google fonts baked in — Barlow Condensed + Open Sans for `showtext`
- [ ] Run `Rscript scripts/setup_local.R --pdf` inside the image build to pre-install CRAN packages + TinyTeX.

### 2. Application layout in the container

- [ ] `WORKDIR /app`
- [ ] Copy project source (`R/`, `reports/`, `config/`, `assets/`, `scripts/`, `game_ids.csv`).
- [ ] **Do not** bake `data_sample/` or `output/` into the image (too large, match-specific).
- [ ] Create empty mount points: `/app/data_sample`, `/app/data/processed`, `/app/output`.

### 3. Credentials & secrets

- [ ] Add `.env.example` documenting required variables:

  ```env
  # Optional: StatsBomb / SDC API (future data download)
  STATSBOMB_USERNAME=
  STATSBOMB_PASSWORD=

  # Match defaults
  SDC_PRIMARY_MATCH_ID=4036731
  SDC_REPORT_FORMAT=both
  ```

- [ ] Add `.env` to `.gitignore` (verify not committed).
- [ ] Load secrets via `docker compose` `env_file: .env` — never hard-code passwords in the Dockerfile.
- [ ] If match JSON is delivered via S3/GCS, add optional download script gated on `AWS_*` or `GCS_*` env vars.

### 4. `docker-compose.yml`

- [ ] Service `sdc-wc` with:
  - [ ] Volume mounts: `./data_sample:/app/data_sample:ro`, `./output:/app/output`, `./data/processed:/app/data/processed`
  - [ ] `env_file: .env`
  - [ ] Default command: `Rscript scripts/run_all.R`
- [ ] Optional service `sdc-wc-interactive` exposing RStudio on port 8787 for development.

### 5. Entrypoint script

- [ ] Create `scripts/docker_entrypoint.sh`:
  1. Validate `data_sample/matches/` is non-empty (fail fast with clear message).
  2. Run `setup_local.R --check` or `--pdf` if needed.
  3. Run `run_build.R` → `run_report.R` with `SDC_PRIMARY_MATCH_ID` and `SDC_REPORT_FORMAT`.
  4. Exit 0 and print paths to `output/reports/` and `output/figures/`.

### 6. CI / publishing

- [ ] GitHub Actions workflow: build image, push to GHCR on tag.
- [ ] Document `docker pull` + `docker compose run` in README once image is published.
- [ ] Smoke test: render HTML for match `4036731` using a minimal fixture dataset in `tests/fixtures/`.

### 7. Documentation

- [ ] README section **Running with Docker** (link here until done).
- [ ] Document disk space needs (~2 GB with TinyTeX; more with full `data_sample`).
- [ ] Document that first PDF build is slow (LaTeX font cache).

---

## Suggested file tree (when implemented)

```
sdc_wc/
├── Dockerfile
├── docker-compose.yml
├── .env.example
├── .dockerignore          # exclude data_sample, output, .git
└── scripts/
    └── docker_entrypoint.sh
```

---

## Open questions

1. Will SDC provide a StatsBomb API token for automated JSON download, or is `data_sample/` always copied manually?
2. Should the image include RStudio Server, or CLI-only is enough?
3. Target registry: Docker Hub, GHCR, or private SDC registry?

---

## References

- [rocker-project.org](https://rocker-project.org/) — official R Docker images
- `scripts/setup_local.R` — package list and directory checks to mirror in Docker
- `README.md` — local prerequisites and workflow
