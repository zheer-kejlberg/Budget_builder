# Budget Builder

A browser-based budget planning tool built with R and Shiny that helps manage complex project budgets with support for multiple salary calculations, category management, templates, and flexible amount definitions.

## Features

### Core Functionality
- **Budget Period Management**: Define budget start/end dates and apply yearly salary inflation
- **Post Management**: Add, edit, and delete budget posts with flexible amount definitions (constant values, formulas, or variable amounts)
- **Salary Calculations**: Pre-calculate salary amounts with pension, holiday allowance, and multiple calculation modes
- **Budget Overview**: View and filter budget posts by various dimensions (period, name, center, category)
- **Import/Export**: Round-trip export to Excel (.xlsx) with full state persistence

### New Features (v2.0)

#### 1. Custom Workbook Names (Issue #3)
- **Set your workbook name** at the top of the app
- **Auto-generated filenames** when exporting (e.g., "MyProject.xlsx" instead of generic "Budget.xlsx")
- **Automatic restoration** of workbook name when importing previously saved files
- **Validation** prevents invalid characters and enforces reasonable length limits

#### 2. Category & Template Management (Issue #2)
A dedicated settings tab allows you to:
- **Create custom categories** with optional validation rules (amount must be > X per month, etc.)
- **Manage templates** with default values that pre-fill the post form
- **Clear/reset** all categories or templates to defaults
- **Full persistence** through export/import cycle

##### Category Validation Rules
When creating a category, you can optionally specify:
- **Operator**: <, ≤, =, >, ≥, ≠
- **Amount**: The threshold value
- **Per**: Unit (month, calendar year, project year)

When adding a post with a validated category, if the amount violates the rule, the form displays a red warning. You can either fix the amount or click Add/Update to get a modal asking whether to proceed anyway.

##### Templates
Templates store default values for:
- Category
- Center
- Amount mode (constant/formula/variable)
- Frequency (month/year)
- FTE and note fields

Any defaults left empty won't override the post form values.

#### 3. New Salary Identifier Syntax (Issue #4)
**Old syntax (no longer supported):**
```r
salaries[s1, "total"]
```

**New syntax (use this in formulas):**
```r
s1$total
```

**Identifier generation** is automatic based on salary names:
- "PhD student" → `phd_student`
- "Research Technician" → `research_technician`
- Duplicates get numeric suffixes: `phd_student_1`, `phd_student_2`

Available fields for each salary identifier:
- `base` — Base salary (monthly)
- `pension` — Pension amount (monthly)
- `own_pension` — Employee's pension contribution (monthly)
- `holiday_base` — Holiday allowance base (monthly)
- `holiday` — Holiday allowance (monthly)
- `total_plus_holiday` — Total including holiday allowance (monthly)
- `total_plus_holiday_y` — Total including holiday allowance (yearly)
- `total` — Total salary (monthly, holiday-absence deducted if 'Subtract holiday' is on)
- `total_y` — Total salary (yearly)

**Example formula:**
```r
apply_inflation(s1$total) + apply_inflation(s2$total)
```

## Getting Started

### 1. Launch the App
Open the budget app in your browser. You'll see:
- **Workbook name field** at the top (default: "Budget")
- **Sidebar** with import, budget period, post entry, and export controls
- **Main panel** with three tabs: Post search, Salary calculations, and Categories & Templates

### 2. Set Your Workbook Name
Enter a descriptive name at the top (e.g., "Project Q2 2026"). This will be used as the filename when you export.

### 3. Set Budget Period & Inflation
- Choose start and end dates for your budget
- Optionally set yearly salary inflation percentage

### 4. (Optional) Configure Categories & Templates
Go to the **Categories & Templates** tab to:
- Add custom categories with validation rules
- Create templates with default values
- Or use the built-in defaults

### 5. Add Posts
In the sidebar, use the "Add or edit post" section:
1. Select a **Template** (or use "Custom" for no defaults)
2. Click **Apply template** to populate the form
3. Fill in Post name, Center, Category, dates, FTE, amount details
4. Click **Add / Update**

**Amount Definition Modes:**
- **Constant**: Single value (e.g., `10000`) or expression (e.g., `510000*FTE`)
- **Function**: Expression returning a vector (e.g., `rep((510000*FTE)/12, n)`)
- **Variable**: Individual values for each month or year

### 6. Add Salary Calculations (if needed)
Go to the **Salary calculations** tab:
1. Enter a salary name (e.g., "PhD student")
2. Set base salary, pension mode/rate, employee pension percentage
3. Click **Add / Update salary**

The generated identifier will appear in the help text. Use this identifier in post formulas.

### 7. View & Manage Budget
The **Post search and overview** tab shows:
- All posts aggregated by period (month, calendar year, or project year)
- Filters for post name, center, category, amount range, and date range
- Posts outside the budget period are highlighted in red

### 8. Export & Save
When all posts are valid (no amendments needed):
- Click **Export [Workbook].xlsx**
- A file download starts with your chosen workbook name

### 9. Import & Continue
Later, to continue work:
- Click **Import Budget workbook**
- Select your previously exported .xlsx file
- All data, settings, and workbook name are restored

## Walkthrough Example

### Scenario: Planning a 2-year research project

**Step 1: Set workbook name**
- Enter "ResearchProject2026" as the workbook name

**Step 2: Configure budget**
- Budget start: Jan 1, 2026
- Budget end: Dec 31, 2027
- Salary inflation: 2.5% yearly

**Step 3: Add salary calculations**
1. Add "PhD student" salary: 510,000/year, 19.36% pension
2. Add "Postdoc": 720,000/year, 19.36% pension
3. Identify the generated identifiers (e.g., `phd_student`, `postdoc`)

**Step 4: Create templates**
Go to Categories & Templates tab:
1. Add template "PhD 3yr":
   - Category: "Salary Ph.D. students"
   - Mode: variable, Unit: year
   - Values: 509928, 539376, 568392
2. Add template "Salary with inflation":
   - Category: "Salary, employees"
   - Mode: formula
   - Formula: `apply_inflation_year(phd_student$total_y)`

**Step 5: Add posts**
1. Select "PhD 3yr" template
2. Fill: Post name="PhD Student 1", Center="Lab A", FTE=1
3. Dates: Jan 1 2026 – Dec 31 2028
4. Click Add / Update

**Step 6: Export**
- Click Export ResearchProject2026.xlsx
- Download starts

**Step 7: Review & re-import (next week)**
- File saved to Downloads
- Click Import, select file
- All data restored: workbook name, categories/templates, posts, salaries

## Formula Variables & Helpers

In amount formulas, you have access to:
- `FTE` — Current post FTE
- `n` — Number of months in the post period
- `months` — Vector of month dates
- `fte_monthly_total` — Vector of total FTE across all posts for each month
- `fte_monthly_sum` — Sum of fte_monthly_total
- `inflation_pct` — Yearly inflation percentage
- `inflation_month_factors` — Month-wise inflation multipliers
- `inflation_year_factors` — Year-wise inflation multipliers
- `apply_inflation(value)` — Apply calendar-year inflation to a base value (returns month or year length vector)
- Salary identifiers (e.g., `s1$total`, `phd_student$total_y`)

## Export Format

The exported Excel file contains multiple sheets:

| Sheet | Content |
|-------|---------|
| `by_month` | Budget aggregated by month |
| `by_calendar_year` | Budget aggregated by calendar year |
| `by_project_year` | Budget aggregated by project year |
| `posts` | Raw post data with all details |
| `salaries` | Salary calculations and identifiers |
| `categories` | Custom categories (including deleted ones) |
| `templates` | Custom templates (including deleted ones) |
| `meta` | Metadata: dates, inflation, workbook name, schema version |

## Troubleshooting

### I forgot my workbook name
When you import a file, the workbook name is automatically restored from the meta sheet. If lost, it falls back to the filename (minus .xlsx).

### Category validation isn't working on my formula
Category validation only applies to constant expressions and variable amounts. Formulas are too flexible to validate automatically—use the override modal if needed.

### My identifiers keep changing
Identifiers are regenerated deterministically from salary names. As long as you use the same salary name, the identifier should remain stable.

### The app is slow
If you have many posts (>1000), consider filtering the view by category or date range to improve responsiveness.

## Technical Details

- Built with **R** and **Shiny** using the Shinylive framework (browser-based R via WebAssembly)
- No server backend required—all calculations run in your browser
- Exports are client-side Excel generation using SheetJS
- Deployed as static site via GitHub Pages

## License

[Your license here]

## Support

For issues or feature requests, visit the GitHub Issues page.
