# --- mover HTMLs ---
unlink("html", recursive = TRUE)
dir.create("html")
R.utils::copyDirectory("qmd/html/qmd/", "html")

# --- funções auxiliares ---
strip_yaml_ronly <- function(lines) {
  if (length(lines) == 0) return(lines)
  is_delim <- grepl("^#'\\s*---\\s*$", lines)
  if (!length(is_delim) || !isTRUE(is_delim[1])) return(lines)  # não começa com YAML
  idx <- which(is_delim)
  if (length(idx) < 2) return(lines) # não achou fechamento
  lines[-seq.int(idx[1], idx[2])]
}

make_header <- function(src) {
  c(
    "# =========================================================================",
    paste0("# Arquivo gerado automaticamente de: ", basename(src)),
    paste0("# Data: ", format(Sys.time(), "%d/%m/%Y %H:%M:%S")),
    "# Renif",
    "# Autor: Prof. Dr. Deoclecio Jardim Amorim",
    "# Versão: 1.0",
    "# =========================================================================",
    ""
  )
}

write_with_header <- function(header, body, path) {
  tmp <- tempfile(pattern = paste0(basename(path), "_"), tmpdir = dirname(path), fileext = ".tmp")
  con <- file(tmp, open = "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)  # fecha UMA vez ao sair
  writeLines(header, con, useBytes = TRUE)
  if (length(body)) writeLines(body, con, useBytes = TRUE)
  # não chame close(con) aqui; o on.exit já vai fechar
  on.exit(NULL)  # cancela o on.exit após fechar manualmente? (não necessário se não fechar)
  # fecha explicitamente e remove o on.exit:
  try(close(con), silent = TRUE)
  # substitui destino de forma portável
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.copy(tmp, path, overwrite = TRUE)
  unlink(tmp)
}

# --- purl + limpeza + cabeçalho ---
qmd_root <- "qmd"
out_root <- "R/r_code_qmd"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

qmd_files <- list.files(qmd_root, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)

for (f in qmd_files) {
  rel <- sub(paste0("^", qmd_root, "/?"), "", f)
  out_dir <- file.path(out_root, dirname(rel))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, sub("\\.qmd$", ".R", basename(f)))
  
  # 1) extrai com comentários dos chunks
  knitr::purl(input = f, output = out_file, documentation = 2)
  
  # 2) lê e remove YAML roxygen do topo (se existir)
  linhas <- readLines(out_file, warn = FALSE, encoding = "UTF-8")
  linhas <- strip_yaml_ronly(linhas)
  
  # 3) escreve com cabeçalho padrão
  header <- make_header(f)
  write_with_header(header, linhas, out_file)
  
  # 4) verificação rápida
  first <- tryCatch(readLines(out_file, n = 1, warn = FALSE), error = function(e) "")
  if (!startsWith(ifelse(length(first) == 0, "", first), "# =")) {
    warning("Cabeçalho não encontrado em: ", out_file)
  } else {
    message(">> Código extraído (limpo + cabeçalho): ", out_file)
  }
}

