Plaquemine <- Plaquemine[!Date %in% 'Sep-Jun']
Plaquemine[, Date := ifelse(Date == '44364', 'Jun', 'Sep')]
Plaquemine[, Treatment := factor(Treatment, levels = c('Control', 'Biochar'))]
factor_cols <- c('Date', 'Block', 'Row', 'Replication')
Plaquemine[, (factor_cols) := lapply(.SD, factor), .SDcols = factor_cols]

Plaquemine_long <- melt(Plaquemine, id.vars = c('Location', 'Date', 'Treatment', 'Block', 'Row','Replication'))
Plaquemine_bydate <- dcast(Plaquemine_long, Location + Treatment + Block + Row + Replication + variable ~ Date)
vmdb = Plaquemine_long[variable == "VM db"]

DiD_vmdb <- lmerTest::lmer(value ~ Treatment + Date + Treatment:Date + (1|Block) + (1|Row), data = vmdb)

postonly_vmdb <- lmerTest::lmer(value ~ Treatment + (1|Block) + (1|Row), data = vmdb, subset = Date == "Sep")

DiD_vmdb_orig <- lmerTest::lmer(log(VM_db) ~ Treatment + Date + Treatment:Date + (1|Block) + (1|Row), data = Plaquemine)
