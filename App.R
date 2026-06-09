library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(DT)
library(scales)

data_path <- file.path(getwd(), "Personal_Budget_Transactions_Dataset.csv")

# persistent settings path
settings_path <- file.path(getwd(), "spendwise_settings.rds")


transactions_raw <- read_csv(
	data_path,
	show_col_types = FALSE,
	progress = FALSE
)

transactions <- transactions_raw |>
	mutate(
		date = ymd_hms(date, tz = "UTC"),
		transaction_type = "Expense",
		category = as.character(category),
		category = trimws(category),
		category = ifelse(category == "", "Uncategorized", category),
		category = case_when(
			tolower(category) == "restuarant" ~ "Restaurant",
			tolower(category) == "coffe" ~ "Coffee",
			TRUE ~ tools::toTitleCase(tolower(category))
		),
		amount = as.numeric(amount)
	) |>
	filter(!is.na(date), !is.na(amount)) |>
	arrange(date) |>
	mutate(
		day = as.Date(date),
		month = floor_date(date, "month")
	)

categories <- sort(unique(transactions$category))
transaction_types <- sort(unique(transactions$transaction_type))
date_range_default <- range(transactions$day, na.rm = TRUE)

theme_spendwise <- bs_theme(
	version = 5,
	bootswatch = "flatly",
	primary = "#1d4ed8",
	secondary = "#2563eb"
)

# Default settings
default_settings <- list(
	currency = "USD",
	default_range = "All",
	round_amounts = FALSE
)

utils::globalVariables(c(
	"date",
	"transaction_type",
	"category",
	"amount",
	"day",
	"month",
	"total",
	"fraction",
	"ymax",
	"ymin",
	"label"
))

app_css <- "
	body {
		background: linear-gradient(180deg, #f8fbff 0%, #eef4ff 100%);
		color: #0f172a;
	}
	.app-shell {
		min-height: 100vh;
	}
	.sidebar-panel {
		background: linear-gradient(180deg, #0f172a 0%, #1e3a8a 100%);
		color: white;
		border-radius: 24px;
		padding: 24px;
		box-shadow: 0 20px 40px rgba(15, 23, 42, 0.18);
	}
	.brand-badge {
		display: inline-flex;
		align-items: center;
		gap: 10px;
		font-weight: 700;
		font-size: 1.05rem;
		margin-bottom: 20px;
	}
	.brand-mark {
		width: 42px;
		height: 42px;
		border-radius: 14px;
		display: inline-flex;
		align-items: center;
		justify-content: center;
		background: rgba(255,255,255,0.14);
		backdrop-filter: blur(10px);
	}
	.nav-stack .nav-link {
		color: rgba(255,255,255,0.82);
		border-radius: 14px;
		margin-bottom: 8px;
		padding: 12px 14px;
		border: 1px solid rgba(255,255,255,0.08);
		background: rgba(255,255,255,0.03);
	}
	.nav-stack .nav-link.active {
		color: #0f172a;
		background: rgba(255,255,255,0.98);
		font-weight: 700;
	}
	.page-title {
		margin-bottom: 6px;
		font-size: 2rem;
		font-weight: 800;
		letter-spacing: -0.03em;
		color: #0f172a;
	}
	.page-subtitle {
		color: #475569;
		margin-bottom: 22px;
	}
	.metric-card {
		background: rgba(255,255,255,0.95);
		border: 1px solid rgba(148,163,184,0.18);
		border-radius: 22px;
		box-shadow: 0 10px 30px rgba(15, 23, 42, 0.08);
		padding: 18px 20px;
		height: 100%;
	}
	.metric-label {
		color: #64748b;
		font-size: 0.9rem;
		font-weight: 600;
		margin-bottom: 8px;
	}
	.metric-value {
		color: #0f172a;
		font-size: 1.7rem;
		font-weight: 800;
		line-height: 1.1;
	}
	.metric-note {
		color: #3b82f6;
		font-size: 0.82rem;
		margin-top: 8px;
	}
	.summary-card {
		background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
		border: 1px solid rgba(148,163,184,0.18);
		border-radius: 20px;
		box-shadow: 0 12px 30px rgba(15, 23, 42, 0.07);
		padding: 18px;
		height: 100%;
	}
	.summary-card-title {
		font-size: 0.82rem;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.08em;
		color: #64748b;
		margin-bottom: 10px;
	}
	.summary-card-value {
		font-size: 1.25rem;
		font-weight: 800;
		color: #0f172a;
		line-height: 1.35;
	}
	.summary-card-note {
		margin-top: 8px;
		color: #475569;
		font-size: 0.92rem;
		line-height: 1.45;
	}
	.insight-box {
		background: #f8fbff;
		border: 1px solid rgba(59,130,246,0.16);
		border-left: 5px solid #1d4ed8;
		border-radius: 18px;
		padding: 16px 18px;
		margin-bottom: 14px;
	}
	.insight-box h5 {
		margin-bottom: 8px;
		font-weight: 800;
		color: #0f172a;
	}
	.status-badge {
		display: inline-flex;
		align-items: center;
		gap: 6px;
		padding: 6px 12px;
		border-radius: 999px;
		font-size: 0.78rem;
		font-weight: 700;
		letter-spacing: 0.02em;
	}
	.status-good {
		background: rgba(16,185,129,0.12);
		color: #047857;
	}
	.status-warn {
		background: rgba(245,158,11,0.14);
		color: #b45309;
	}
	.status-alert {
		background: rgba(239,68,68,0.12);
		color: #b91c1c;
	}
	.status-neutral {
		background: rgba(59,130,246,0.12);
		color: #1d4ed8;
	}
	.summary-table {
		width: 100%;
		border-collapse: collapse;
	}
	.summary-table th,
	.summary-table td {
		padding: 10px 12px;
		border-bottom: 1px solid rgba(148,163,184,0.16);
		text-align: left;
		vertical-align: top;
	}
	.summary-table th {
		color: #475569;
		font-size: 0.8rem;
		text-transform: uppercase;
		letter-spacing: 0.06em;
	}
	.summary-table td {
		color: #0f172a;
		font-size: 0.92rem;
	}
	.transaction-grid {
		display: grid;
		grid-template-columns: repeat(4, minmax(0, 1fr));
		gap: 14px;
		margin-bottom: 16px;
	}
	.transaction-insight {
		background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
		border: 1px solid rgba(148,163,184,0.18);
		border-radius: 18px;
		padding: 16px 18px;
		box-shadow: 0 12px 28px rgba(15, 23, 42, 0.06);
	}
	.transaction-insight .metric-label {
		margin-bottom: 6px;
	}
	.transaction-insight .metric-value {
		font-size: 1.35rem;
	}
	.transaction-insight .metric-note {
		font-size: 0.8rem;
	}
	.transactions-layout {
		display: grid;
		grid-template-columns: 1.3fr 0.7fr;
		gap: 18px;
		align-items: start;
	}
	.transactions-sidecard {
		background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
		border: 1px solid rgba(148,163,184,0.18);
		border-radius: 22px;
		box-shadow: 0 12px 30px rgba(15, 23, 42, 0.07);
		padding: 18px;
	}
	.transactions-sidecard h4 {
		margin-bottom: 12px;
	}
	.transaction-list {
		list-style: none;
		padding: 0;
		margin: 0;
	}
	.transaction-list li {
		padding: 10px 0;
		border-bottom: 1px solid rgba(148,163,184,0.16);
		color: #334155;
		font-size: 0.93rem;
		line-height: 1.45;
	}
	.transaction-list li:last-child {
		border-bottom: none;
	}
	.transactions-table-wrap {
		background: #ffffff;
		border-radius: 22px;
		padding: 16px;
		box-shadow: 0 16px 40px rgba(15, 23, 42, 0.08);
		border: 1px solid rgba(148,163,184,0.18);
		overflow-x: auto;
	}
	.transactions-table-wrap .dt-container {
		width: 100% !important;
	}
	.transactions-table-wrap .dataTables_wrapper {
		width: 100% !important;
	}
	.panel-card {
		background: rgba(255,255,255,0.97);
		border: 1px solid rgba(148,163,184,0.18);
		border-radius: 24px;
		box-shadow: 0 16px 40px rgba(15, 23, 42, 0.08);
		padding: 18px;
		margin-bottom: 18px;
	}
	.section-heading {
		font-size: 1.05rem;
		font-weight: 700;
		color: #0f172a;
		margin-bottom: 14px;
	}
	.table-card .dt-container {
		background: transparent;
	}
	.nav-tabs > li > a, .nav-tabs > li > a:hover {
		border-radius: 999px;
	}
	.small-kicker {
		text-transform: uppercase;
		letter-spacing: 0.12em;
		color: #3b82f6;
		font-size: 0.72rem;
		font-weight: 700;
	}
"

nav_items <- list(
	dashboard = list(icon = icon("chart-column"), label = "Dashboard"),
	transactions = list(icon = icon("table"), label = "Transactions"),
	analysis = list(icon = icon("chart-line"), label = "Spending Analysis"),
	prediction = list(icon = icon("calendar-check"), label = "Budget Prediction"),
	reports = list(icon = icon("file-lines"), label = "Reports"),
	settings = list(icon = icon("gear"), label = "Settings")
)

sidebar_button <- function(id, item) {
	actionLink(
		inputId = paste0("nav_", id),
		label = tagList(item$icon, span(item$label)),
		class = "nav-link w-100 text-start"
	)
}

metric_card <- function(label, value, note = NULL) {
	div(
		class = "metric-card",
		div(class = "metric-label", label),
		div(class = "metric-value", value),
		if (!is.null(note)) div(class = "metric-note", note)
	)
}

summary_card <- function(title, value, note = NULL) {
	div(
		class = "summary-card",
		div(class = "summary-card-title", title),
		div(class = "summary-card-value", value),
		if (!is.null(note)) div(class = "summary-card-note", note)
	)
}

status_badge <- function(label, tone = "good") {
	dive_class <- switch(
		tone,
		good = "status-badge status-good",
		warn = "status-badge status-warn",
		alert = "status-badge status-alert",
		neutral = "status-badge status-neutral",
		"status-badge status-neutral"
	)
	span(class = dive_class, label)
}

create_empty_plot <- function(title) {
	ggplot() +
		annotate("text", x = 0, y = 0, label = title, size = 5, fontface = "bold", color = "#1e3a8a") +
		theme_void()
}

ui <- page_sidebar(
	title = NULL,
	theme = theme_spendwise,
	fillable = TRUE,
	sidebar = sidebar(
		width = 310,
		class = "sidebar-panel",
		tags$div(
			class = "brand-badge",
			tags$span(class = "brand-mark", icon("wallet")),
			tags$span("SpendWise")
		),
		tags$p("Personal Financial Analytics and Budget Prediction System", style = "color: rgba(255,255,255,0.78); margin-bottom: 22px; line-height: 1.5;"),
		div(
			class = "nav-stack",
			sidebar_button("dashboard", nav_items$dashboard),
			sidebar_button("transactions", nav_items$transactions),
			sidebar_button("analysis", nav_items$analysis),
			sidebar_button("prediction", nav_items$prediction),
			sidebar_button("reports", nav_items$reports),
			sidebar_button("settings", nav_items$settings)
		),
		div(
			class = "panel-card",
			style = "margin-top: 18px; background: rgba(255,255,255,0.96); color: #0f172a;",
			h4("Filters", class = "section-heading"),
			uiOutput("date_range_ui"),
			uiOutput("category_ui"),
			uiOutput("type_ui")
		)
	),
	layout_column_wrap(
		width = 1,
		navset_hidden(
			id = "main_nav",
			selected = "Dashboard",
			nav_panel(
				title = "Dashboard",
				div(
					class = "app-shell",
					div(class = "small-kicker", "Financial Snapshot"),
					div(class = "page-title", "Personal Financial Analytics and Budget Prediction System"),
					div(class = "page-subtitle", "Monitor spending behavior, compare categories, and prepare for future budgeting decisions."),
					layout_column_wrap(
						width = 1 / 4,
						metric_card("Total Transactions", textOutput("total_transactions", inline = TRUE)),
						metric_card("Total Expenses", textOutput("total_expenses", inline = TRUE)),
						metric_card("Average Transaction Amount", textOutput("average_amount", inline = TRUE)),
						metric_card("Highest Spending Category", textOutput("highest_category", inline = TRUE))
					),
					div(class = "panel-card", plotOutput("bar_category", height = "340px")),
					layout_columns(
						col_widths = c(6, 6),
						div(class = "panel-card", plotOutput("line_trend", height = "320px")),
						div(class = "panel-card", plotOutput("donut_category", height = "320px"))
					),
					div(class = "panel-card", plotOutput("hist_amounts", height = "320px"))
				)
			),
			nav_panel(
				title = "Transactions",
				div(
					class = "app-shell",
					div(class = "small-kicker", "Transaction Table"),
					div(class = "page-title", "Transactions Overview"),
					div(class = "page-subtitle", "A concise summary of the filtered transactions with a compact, presentation-ready table."),
					div(
						class = "transaction-grid",
						metric_card("Filtered Records", textOutput("transactions_count", inline = TRUE), "Rows currently visible in the active slice"),
						metric_card("Total Spending", textOutput("transactions_total_spend", inline = TRUE), "Aggregate amount in the current filter set"),
						metric_card("Average Amount", textOutput("transactions_avg_amount", inline = TRUE), "Average transaction size"),
						metric_card("Top Category", textOutput("transactions_top_category", inline = TRUE), "Highest spending category in the current view")
					),
					div(
						class = "transactions-layout",
						div(
							class = "transactions-table-wrap",
							div(class = "section-heading", "Recent Transactions"),
							DTOutput("transactions_table")
						),
						div(
							class = "transactions-sidecard",
							h4("Quick Insights", class = "section-heading"),
							tags$ul(
								class = "transaction-list",
									tags$li("The table is filtered by the sidebar controls and updates immediately when the date range or category changes."),
									tags$li("Use the summary cards to confirm whether the current slice is concentrated in one category or spread across several."),
									tags$li("This layout is optimized for review and presentation, not chart-heavy exploration."),
									tags$li("Export the filtered table from the Settings page when you need a presentation or appendix data extract.")
							)
						)
					)
				)
			),
			nav_panel(
				title = "Spending Analysis",
				div(
					class = "app-shell",
					div(class = "small-kicker", "Deep Dive"),
					div(class = "page-title", "Spending Analysis"),
					div(class = "page-subtitle", "Use the filters to isolate a period, category, or transaction type."),
					layout_column_wrap(
						width = 1 / 3,
						div(class = "panel-card", plotOutput("analysis_bar", height = "300px")),
						div(class = "panel-card", plotOutput("analysis_line", height = "300px")),
						div(class = "panel-card", plotOutput("analysis_donut", height = "300px"))
					)
				)
			),
			nav_panel(
				title = "Budget Prediction",
				div(
					class = "app-shell",
					div(class = "small-kicker", "Forecasting"),
					div(class = "page-title", "Budget Prediction"),
					div(class = "page-subtitle", "A simple monthly projection provides an interpretable baseline for budgeting conversations."),
					layout_column_wrap(
						width = 1 / 3,
						metric_card("Projected Next Month", textOutput("projected_budget", inline = TRUE)),
						metric_card("Average Monthly Spend", textOutput("monthly_average", inline = TRUE)),
						metric_card("Observed Months", textOutput("observed_months", inline = TRUE))
					),
					div(class = "panel-card", plotOutput("forecast_plot", height = "360px"))
				)
			),
			nav_panel(
				title = "Reports",
				div(
					class = "app-shell",
					div(class = "small-kicker", "Summary Reports"),
					div(class = "page-title", "Executive Summary"),
					div(class = "page-subtitle", "Concise findings for a capstone presentation, built from the active filters."),
					div(
						class = "panel-card",
						uiOutput("report_summary")
					)
				)
			),
			nav_panel(
				title = "Settings",
				div(
					class = "app-shell",
					div(class = "small-kicker", "Configuration"),
					div(class = "page-title", "Settings"),
					div(class = "page-subtitle", "Adjust dashboard filters and display preferences."),
					div(
						class = "panel-card",
						h4("Appearance & Preferences", class = "section-heading"),
						selectInput("settings_currency", "Currency", choices = c("USD", "EUR", "PHP", "JPY"), selected = default_settings$currency, width = "100%"),
						checkboxInput("round_amounts", "Round amounts in table", value = default_settings$round_amounts),
						hr(),
						h4("Defaults & Actions", class = "section-heading"),
						selectInput("settings_default_range", "Default date range for filters", choices = c("All", "Last 30 days", "Last 90 days", "Last 1 year"), selected = default_settings$default_range, width = "100%"),
						actionButton("apply_settings", "Apply & Save Settings", class = "btn-primary", style = "margin-top:10px;"),
						br(), br(),
						downloadButton("export_filtered", "Export Filtered CSV", class = "btn-secondary")
					)
				)
			)
		)
	),
	tags$head(
		tags$style(HTML(app_css))
	)
)

server <- function(input, output, session) {
	observeEvent(input$nav_dashboard, nav_select("main_nav", selected = "Dashboard", session = session))
	observeEvent(input$nav_transactions, nav_select("main_nav", selected = "Transactions", session = session))
	observeEvent(input$nav_analysis, nav_select("main_nav", selected = "Spending Analysis", session = session))
	observeEvent(input$nav_prediction, nav_select("main_nav", selected = "Budget Prediction", session = session))
	observeEvent(input$nav_reports, nav_select("main_nav", selected = "Reports", session = session))
	observeEvent(input$nav_settings, nav_select("main_nav", selected = "Settings", session = session))

	output$date_range_ui <- renderUI({
		dateRangeInput(
			"date_range",
			"Date Range",
			start = date_range_default[1],
			end = date_range_default[2],
			min = date_range_default[1],
			max = date_range_default[2],
			width = "100%"
		)
	})

	# --- Settings state: load saved settings if present ---
	settings_rv <- reactiveValues()
	# Load saved settings into a local variable first to avoid reading reactiveValues
	initial_settings <- NULL
	if (file.exists(settings_path)) {
		initial_settings <- tryCatch(readRDS(settings_path), error = function(e) NULL)
	}
	if (is.null(initial_settings)) initial_settings <- default_settings
	settings_rv$data <- initial_settings

	currency_symbol <- reactive({
		cur <- if (!is.null(input$settings_currency)) input$settings_currency else settings_rv$data$currency
		switch(cur, USD = "$", EUR = "€", PHP = "₱", JPY = "¥", "$")
	})

	# Reactive accuracy for rounding amounts
	accuracy_val <- reactive({
		if (!is.null(input$round_amounts)) {
			if (isTRUE(input$round_amounts)) 1 else 0.01
		} else {
			if (!is.null(isolate(settings_rv$data$round_amounts)) && isTRUE(isolate(settings_rv$data$round_amounts))) 1 else 0.01
		}
	})

	# Currency formatter for plot labels
	currency_formatter <- reactive({
		scales::dollar_format(prefix = currency_symbol(), accuracy = accuracy_val())
	})

	observeEvent(input$apply_settings, {
		new_settings <- list(
			currency = input$settings_currency,
			default_range = input$settings_default_range,
			round_amounts = isTRUE(input$round_amounts)
		)
		settings_rv$data <- new_settings
		try(saveRDS(settings_rv$data, settings_path), silent = TRUE)
		showNotification("Settings saved", type = "message")
	})

	output$export_filtered <- downloadHandler(
		filename = function() paste0("filtered_transactions_", Sys.Date(), ".csv"),
		content = function(file) {
			write.csv(filtered_data(), file, row.names = FALSE)
		}
	)

	output$category_ui <- renderUI({
		selectInput(
			"category_filter",
			"Expense Category",
			choices = c("All Categories", categories),
			selected = "All Categories",
			width = "100%"
		)
	})

	output$type_ui <- renderUI({
		selectInput(
			"type_filter",
			"Transaction Type",
			choices = c("All Types", transaction_types),
			selected = "All Types",
			width = "100%"
		)
	})

	filtered_data <- reactive({
		req(input$date_range)
		data <- transactions |>
			filter(day >= input$date_range[1], day <= input$date_range[2])

		if (!is.null(input$category_filter) && input$category_filter != "All Categories") {
			data <- data |> filter(category == input$category_filter)
		}

		if (!is.null(input$type_filter) && input$type_filter != "All Types") {
			data <- data |> filter(transaction_type == input$type_filter)
		}

		data
	})

	summary_metrics <- reactive({
		data <- filtered_data()

		category_totals <- data |>
			group_by(category) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
			arrange(desc(total))

		highest_category <- if (nrow(category_totals) > 0) category_totals$category[1] else "N/A"

		list(
			total_transactions = nrow(data),
			total_expenses = sum(data$amount, na.rm = TRUE),
			average_amount = if (nrow(data) > 0) mean(data$amount, na.rm = TRUE) else 0,
			highest_category = highest_category,
			category_totals = category_totals
		)
	})

	output$total_transactions <- renderText({
		comma(summary_metrics()$total_transactions)
	})

	output$total_expenses <- renderText({
		scales::dollar(summary_metrics()$total_expenses, prefix = currency_symbol(), accuracy = accuracy_val())
	})

	output$average_amount <- renderText({
		scales::dollar(summary_metrics()$average_amount, prefix = currency_symbol(), accuracy = accuracy_val())
	})

	output$highest_category <- renderText({
		summary_metrics()$highest_category
	})

	output$bar_category <- renderPlot({
		data <- summary_metrics()$category_totals
		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		ggplot(data, aes(x = reorder(category, total), y = total, fill = category)) +
			geom_col(width = 0.7, show.legend = FALSE) +
			coord_flip() +
			scale_fill_manual(values = colorRampPalette(c("#dbeafe", "#1d4ed8"))(nrow(data))) +
			scale_y_continuous(labels = currency_formatter()) +
			labs(
				title = "Total Spending by Category",
				x = NULL,
				y = NULL
			) +
			theme_minimal(base_size = 13) +
			theme(
				plot.title = element_text(face = "bold", color = "#0f172a"),
				panel.grid.major.y = element_blank(),
				axis.text.y = element_text(color = "#1e293b")
			)
	})

	output$line_trend <- renderPlot({
		data <- filtered_data() |>
			group_by(day) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		ggplot(data, aes(x = day, y = total)) +
			geom_line(color = "#1d4ed8", linewidth = 1.2) +
			geom_point(color = "#2563eb", size = 2) +
			scale_y_continuous(labels = currency_formatter()) +
			labs(
				title = "Spending Trend Over Time",
				x = NULL,
				y = NULL
			) +
			theme_minimal(base_size = 13) +
			theme(
				plot.title = element_text(face = "bold", color = "#0f172a"),
				axis.text.x = element_text(color = "#1e293b")
			)
	})

	output$hist_amounts <- renderPlot({
		data <- filtered_data()
		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		ggplot(data, aes(x = amount)) +
			geom_histogram(bins = 28, fill = "#3b82f6", color = "white", alpha = 0.9) +
			scale_x_continuous(labels = currency_formatter()) +
			labs(
				title = "Distribution of Transaction Amounts",
				x = NULL,
				y = "Count"
			) +
			theme_minimal(base_size = 13) +
			theme(
				plot.title = element_text(face = "bold", color = "#0f172a"),
				axis.text = element_text(color = "#1e293b")
			)
	})

	output$donut_category <- renderPlot({
		data <- summary_metrics()$category_totals
		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		data <- data |>
			mutate(
				fraction = total / sum(total),
				ymax = cumsum(fraction),
				ymin = lag(ymax, default = 0),
				label = paste0(category, " (", percent(fraction, accuracy = 0.1), ")")
			)

		ggplot(data, aes(fill = category, ymax = ymax, ymin = ymin, xmax = 4, xmin = 2)) +
			geom_rect(color = "white", linewidth = 1) +
			coord_polar(theta = "y") +
			xlim(c(0, 4)) +
			scale_fill_manual(values = colorRampPalette(c("#dbeafe", "#1d4ed8"))(nrow(data))) +
			labs(title = "Spending Share by Category") +
			theme_void(base_size = 13) +
			theme(
				plot.title = element_text(face = "bold", color = "#0f172a", hjust = 0.5),
				legend.position = "right"
			)
	})

	output$analysis_bar <- renderPlot({
		data <- summary_metrics()$category_totals
		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		ggplot(data, aes(x = reorder(category, total), y = total, fill = category)) +
			geom_col(width = 0.7, show.legend = FALSE) +
			coord_flip() +
			scale_fill_manual(values = colorRampPalette(c("#bfdbfe", "#1d4ed8"))(nrow(data))) +
			scale_y_continuous(labels = currency_formatter()) +
			labs(title = "Category Spend", x = NULL, y = NULL) +
			theme_minimal(base_size = 12) +
			theme(plot.title = element_text(face = "bold", color = "#0f172a"))
	})

	output$analysis_line <- renderPlot({
		data <- filtered_data() |>
			group_by(day) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		ggplot(data, aes(day, total)) +
			geom_line(color = "#1d4ed8", linewidth = 1.1) +
			geom_smooth(se = FALSE, color = "#93c5fd", linewidth = 0.8, linetype = "dashed") +
			scale_y_continuous(labels = currency_formatter()) +
			labs(title = "Trend", x = NULL, y = NULL) +
			theme_minimal(base_size = 12) +
			theme(plot.title = element_text(face = "bold", color = "#0f172a"))
	})

	output$analysis_donut <- renderPlot({
		data <- summary_metrics()$category_totals
		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		data <- data |>
			mutate(
				fraction = total / sum(total),
				ymax = cumsum(fraction),
				ymin = lag(ymax, default = 0)
			)

		ggplot(data, aes(fill = category, ymax = ymax, ymin = ymin, xmax = 4, xmin = 2)) +
			geom_rect(color = "white", linewidth = 1) +
			coord_polar(theta = "y") +
			xlim(c(0, 4)) +
			scale_fill_manual(values = colorRampPalette(c("#dbeafe", "#1d4ed8"))(nrow(data))) +
			labs(title = "Share") +
			theme_void(base_size = 12) +
			theme(plot.title = element_text(face = "bold", color = "#0f172a", hjust = 0.5))
	})

	monthly_series <- reactive({
		filtered_data() |>
			group_by(month) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
			arrange(month)
	})

	output$projected_budget <- renderText({
		data <- monthly_series()
		if (nrow(data) == 0) return(scales::dollar(0, prefix = currency_symbol(), accuracy = accuracy_val()))

		model <- lm(total ~ as.numeric(month), data = data)
		next_month <- max(data$month) %m+% months(1)
		prediction <- max(0, predict(model, newdata = data.frame(month = as.numeric(next_month))))
		scales::dollar(prediction, prefix = currency_symbol(), accuracy = accuracy_val())
	})

	output$monthly_average <- renderText({
		data <- monthly_series()
		if (nrow(data) == 0) return(scales::dollar(0, prefix = currency_symbol(), accuracy = accuracy_val()))
		scales::dollar(mean(data$total), prefix = currency_symbol(), accuracy = accuracy_val())
	})

	output$observed_months <- renderText({
		nrow(monthly_series())
	})

	output$forecast_plot <- renderPlot({
		data <- monthly_series()
		if (nrow(data) == 0) return(create_empty_plot("No data for the selected filters"))

		model <- lm(total ~ as.numeric(month), data = data)
		next_month <- max(data$month) %m+% months(1)
		future <- data.frame(
			month = c(data$month, next_month),
			total = c(data$total, predict(model, newdata = data.frame(month = as.numeric(next_month))))
		)

		ggplot(data, aes(month, total)) +
			geom_line(color = "#1d4ed8", linewidth = 1.1) +
			geom_point(color = "#1d4ed8", size = 2.4) +
			geom_line(data = future, aes(month, total), color = "#93c5fd", linewidth = 1.1, linetype = "dashed") +
			geom_point(data = tail(future, 1), aes(month, total), color = "#0f172a", size = 3) +
			scale_y_continuous(labels = currency_formatter()) +
			scale_x_date(date_labels = "%b %Y") +
			labs(
				title = "Baseline Monthly Budget Projection",
				x = NULL,
				y = NULL
			) +
			theme_minimal(base_size = 13) +
			theme(plot.title = element_text(face = "bold", color = "#0f172a"))
	})

	output$report_summary <- renderUI({
		data <- filtered_data()
		if (nrow(data) == 0) {
			return(div(
				class = "panel-card",
				div(class = "section-heading", "Executive Summary"),
				p("No transactions match the current filters. Adjust the sidebar filters to view the executive summary.")
			))
		}

		category_totals <- data |>
			group_by(category) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
			arrange(desc(total))

		monthly_totals <- data |>
			group_by(month) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
			arrange(month)

		total_spending <- sum(data$amount, na.rm = TRUE)
		average_amount_value <- mean(data$amount, na.rm = TRUE)
		top_category <- if (nrow(category_totals) > 0) category_totals$category[1] else "N/A"
		top_category_value <- if (nrow(category_totals) > 0) category_totals$total[1] else 0

		trend_text <- "Not enough monthly history to estimate a direction."
		trend_tone <- "neutral"
		if (nrow(monthly_totals) >= 2) {
			first_total <- monthly_totals$total[1]
			last_total <- monthly_totals$total[nrow(monthly_totals)]
			change_pct <- if (first_total > 0) ((last_total - first_total) / first_total) * 100 else NA_real_
			if (!is.na(change_pct)) {
				trend_tone <- if (abs(change_pct) < 5) "neutral" else if (change_pct > 0) "warn" else "good"
				trend_text <- paste0(
					"Spending moved from ",
					scales::dollar(first_total, prefix = currency_symbol(), accuracy = accuracy_val()),
					" to ",
					scales::dollar(last_total, prefix = currency_symbol(), accuracy = accuracy_val()),
					" across the selected period (",
					format(round(change_pct, 1), nsmall = 1),
					"% change)."
				)
			}
		}

		spend_share <- if (total_spending > 0) (top_category_value / total_spending) * 100 else 0
		volatility_ratio <- if (mean(monthly_totals$total) > 0) sd(monthly_totals$total) / mean(monthly_totals$total) else 0

		quality_rows <- data.frame(
			Check = c("Parsed dates", "Valid amounts", "Category standardization", "Filtered dataset ready"),
			Status = c(
				if (all(!is.na(data$date))) "Complete" else "Review",
				if (all(!is.na(data$amount))) "Complete" else "Review",
				"Applied during preprocessing",
				paste0(nrow(data), " rows available")
			),
			Details = c(
				"All records in the active view contain parsed dates.",
				"Amount values are numeric after preprocessing.",
				"Category names were trimmed and normalized to title case.",
				"The executive summary refreshes as filters change."
			),
			stringsAsFactors = FALSE
		)

		model_rows <- data.frame(
			Criterion = c("Sample size", "Monthly coverage", "Category diversity", "Feature signal"),
			Assessment = c(
				if (nrow(data) >= 25) "Strong" else "Limited",
				if (nrow(monthly_totals) >= 6) "Strong" else "Needs more history",
				if (nrow(category_totals) >= 5) "Strong" else "Moderate",
				if (volatility_ratio > 0.15) "Useful variation" else "Limited variation"
			),
			Recommendation = c(
				"Sufficient for baseline supervised modeling.",
				"Monthly forecasting is acceptable for a capstone demo.",
				"Category features can support segmentation or classification.",
				"Consider spending anomalies and seasonality features."
			),
			stringsAsFactors = FALSE
		)

		recommendation_rows <- data.frame(
			Priority = c("1", "2", "3"),
			Recommendation = c(
				paste0("Focus budget controls on ", top_category, ", which accounts for ", format(round(spend_share, 1), nsmall = 1), "% of filtered spend."),
				"Use the monthly trend direction to set practical spending caps for the next cycle.",
				"Treat the current dataset as ready for an entry-level predictive model, then expand it with more months for better accuracy."
			),
			stringsAsFactors = FALSE
		)

		make_table <- function(df) {
			cols <- names(df)
			tags$table(
				class = "summary-table",
				tags$thead(
					tags$tr(lapply(cols, tags$th))
				),
				tags$tbody(
					lapply(seq_len(nrow(df)), function(i) {
						tags$tr(lapply(cols, function(col) tags$td(as.character(df[[col]][i]))))
					})
				)
			)
		}

		summary_cards <- div(
			class = "panel-card",
			div(
				class = "section-heading",
				"Executive Snapshot"
			),
			layout_column_wrap(
				width = 1 / 4,
				summary_card(
					"Highest Spending Category",
					 top_category,
					 paste0(scales::dollar(top_category_value, prefix = currency_symbol(), accuracy = accuracy_val()), " total filtered spend")
				),
				summary_card(
					"Total Spending",
					scales::dollar(total_spending, prefix = currency_symbol(), accuracy = accuracy_val()),
					paste0(nrow(data), " filtered transactions")
				),
				summary_card(
					"Average Transaction",
					scales::dollar(average_amount_value, prefix = currency_symbol(), accuracy = accuracy_val()),
					"Useful for setting transaction-level controls"
				),
				summary_card(
					"Model Readiness",
					if (nrow(data) >= 25 && nrow(monthly_totals) >= 6) "Ready for baseline ML" else "Needs more history",
					"Assesses whether the current slice is strong enough for a first-pass model"
				)
			)
		)

		insight_boxes <- div(
			class = "panel-card",
			div(class = "section-heading", "Analytical Findings"),
			div(
				class = "insight-box",
				h5("Spending Trend Summary"),
				p(trend_text),
				status_badge(if (trend_tone == "good") "Favorable" else if (trend_tone == "warn") "Watch closely" else "Stable", tone = trend_tone)
			),
			div(
				class = "insight-box",
				h5("Budget Planning Insights"),
				p(paste0(
					"The largest category is ", top_category, ", representing ",
					format(round(spend_share, 1), nsmall = 1),
					"% of filtered spending. Use it as the primary target for budget limits and variance monitoring."
				)),
				status_badge(if (spend_share >= 40) "Concentrated spend" else "Balanced mix", tone = if (spend_share >= 40) "warn" else "good")
			),
			div(
				class = "insight-box",
				h5("Data Quality and Preprocessing Summary"),
				p("Raw transaction fields were standardized by trimming category names, fixing obvious spelling variants, parsing dates, and removing invalid rows with missing dates or amounts."),
				status_badge("Preprocessing complete", tone = "good")
			),
			div(
				class = "insight-box",
				h5("Model Readiness Assessment"),
				p(if (nrow(data) >= 25 && nrow(monthly_totals) >= 6) {
					"The current slice is suitable for a baseline model such as regression or simple forecasting, especially for capstone demonstration purposes."
				} else {
					"The current slice is better for descriptive analysis than modeling; gather more months or more categories before training a stronger predictive model."
				}),
				status_badge(if (nrow(data) >= 25 && nrow(monthly_totals) >= 6) "Ready" else "Needs more data", tone = if (nrow(data) >= 25 && nrow(monthly_totals) >= 6) "good" else "alert")
			)
		)

		final_recommendations <- div(
			class = "panel-card",
			div(class = "section-heading", "Final Recommendations"),
			make_table(recommendation_rows)
		)

		details_grid <- layout_column_wrap(
			width = 1 / 2,
			div(class = "panel-card", div(class = "section-heading", "Data Quality Table"), make_table(quality_rows)),
			div(class = "panel-card", div(class = "section-heading", "Model Readiness Table"), make_table(model_rows))
		)

		tags$div(
			summary_cards,
			insight_boxes,
			details_grid,
			final_recommendations
		)
	})

	output$transactions_table <- renderDT({
		# Respect user currency and rounding preference
		req(currency_symbol())
		data <- filtered_data() |>
			arrange(desc(date)) |>
			transmute(
				Date = format(date, "%Y-%m-%d %H:%M"),
				Category = category,
				Amount = scales::dollar(amount, prefix = currency_symbol(), accuracy = accuracy_val()),
				`Transaction Type` = transaction_type
			)

		datatable(
			data,
			rownames = FALSE,
			class = "stripe hover nowrap",
			options = list(
				pageLength = 10,
				autoWidth = TRUE,
				scrollX = TRUE,
				responsive = TRUE,
				dom = "tip"
			)
		)
	})

	output$transactions_count <- renderText({
		comma(nrow(filtered_data()))
	})

	output$transactions_total_spend <- renderText({
		scales::dollar(sum(filtered_data()$amount, na.rm = TRUE), prefix = currency_symbol(), accuracy = accuracy_val())
	})

	output$transactions_avg_amount <- renderText({
		data <- filtered_data()
		if (nrow(data) == 0) return(scales::dollar(0, prefix = currency_symbol(), accuracy = accuracy_val()))
		scales::dollar(mean(data$amount, na.rm = TRUE), prefix = currency_symbol(), accuracy = accuracy_val())
	})

	output$transactions_top_category <- renderText({
		data <- filtered_data()
		if (nrow(data) == 0) return("N/A")
		category_totals <- data |>
			group_by(category) |>
			summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
			arrange(desc(total))
		if (nrow(category_totals) == 0) "N/A" else category_totals$category[1]
	})
}

shinyApp(ui, server)
