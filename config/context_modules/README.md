# Xonsh Context Module

## Setup für vollständige History mit Output & Duration

Um **Output** und **Duration** in der xonsh History zu speichern, setze folgende Environment Variables in deiner `~/.xonshrc`:

```python
$XONSH_STORE_STDOUT = True      # Speichere captured Output
$XONSH_CAPTURE_ALWAYS = True    # Capture ALLE Commands (nicht nur $())
```

### Was wird gespeichert:

- **inp**: Der eingegebene Command
- **rtn**: Return Code (0 = Erfolg)
- **ts**: Timestamps [start, end] → Duration wird berechnet!
- **out**: Output des Commands (nur wenn beide Variablen aktiviert sind)
- **cwd**: Working Directory beim Command

### Warum beide Variablen?

- **`$XONSH_STORE_STDOUT = True`**: Wenn Output captured wird, speichere ihn in der History
- **`$XONSH_CAPTURE_ALWAYS = True`**: Capture Output von ALLEN Commands automatisch

**Ohne `$XONSH_CAPTURE_ALWAYS`:**
- `ifconfig` → kein Output gespeichert
- `$(ifconfig)` → Output gespeichert (explizit captured)

**Mit `$XONSH_CAPTURE_ALWAYS`:**
- `ifconfig` → Output gespeichert ✅
- Alle Commands werden captured und gespeichert

### Verwendung

Das `xonsh.xsh` Context Modul liest automatisch:

1. **Live Session** (`__xonsh__.history`): Wenn in der aktuellen xonsh Session ausgeführt
   - Hat ALLE Daten: Commands, Return Codes, Timestamps, Duration, Output, CWD
   
2. **Fallback** (SQLite/JSON direkt): Wenn als subprocess ausgeführt (z.B. von AIsh)
   - Lädt History aus allen Sessions
   - Zeigt die letzten 100 Commands mit allen verfügbaren Daten

### Performance

⚠️ **Warnung:** Diese Einstellungen können:
- Die History-Datei stark vergrößern (viel Output!)
- Interaktive Commands beeinträchtigen (z.B. `vim`, `less`)

**Empfohlen für:**
- Development/Debugging Sessions
- AI-Agent Context (maximaler Kontext!)

**Nicht empfohlen für:**
- Production Systeme
- Commands mit sehr viel Output

### Test

```bash
$XONSH_STORE_STDOUT = True
$XONSH_CAPTURE_ALWAYS = True
ls -la  # Führe einen Command aus
__xonsh__.history[-1].out  # Zeige Output vom letzten Command
```
