-- Add campaign system tables to org-206.sqlite3

CREATE TABLE IF NOT EXISTS campaign_types (
  name TEXT PRIMARY KEY,
  respect_exclusion_windows BOOLEAN DEFAULT TRUE,
  enable_followups BOOLEAN DEFAULT TRUE,
  days_before_event INTEGER DEFAULT 0,
  target_all_contacts BOOLEAN DEFAULT FALSE,
  priority INTEGER DEFAULT 10,
  active BOOLEAN DEFAULT TRUE,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS campaign_instances (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  campaign_type TEXT NOT NULL,
  instance_name TEXT NOT NULL,
  email_template TEXT,
  sms_template TEXT,
  active_start_date DATE,
  active_end_date DATE,
  metadata TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(campaign_type, instance_name),
  FOREIGN KEY (campaign_type) REFERENCES campaign_types(name)
);

CREATE TABLE IF NOT EXISTS contact_campaigns (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  contact_id INTEGER NOT NULL,
  campaign_instance_id INTEGER NOT NULL,
  trigger_date DATE,
  status TEXT DEFAULT 'pending',
  metadata TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(contact_id, campaign_instance_id, trigger_date),
  FOREIGN KEY (campaign_instance_id) REFERENCES campaign_instances(id),
  FOREIGN KEY (contact_id) REFERENCES contacts(id)
);

CREATE TABLE IF NOT EXISTS campaign_change_log (
  id INTEGER PRIMARY KEY,
  campaign_instance_id INTEGER NOT NULL,
  field_changed TEXT NOT NULL,
  old_value TEXT,
  new_value TEXT,
  changed_at DATETIME NOT NULL,
  changed_by TEXT,
  requires_rescheduling BOOLEAN DEFAULT TRUE,
  FOREIGN KEY (campaign_instance_id) REFERENCES campaign_instances(id)
);

CREATE TABLE IF NOT EXISTS scheduler_checkpoints (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  scheduler_run_id TEXT NOT NULL UNIQUE,
  contacts_processed INTEGER DEFAULT 0,
  schedules_created INTEGER DEFAULT 0,
  started_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  last_updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Add indexes for performance
CREATE INDEX IF NOT EXISTS idx_campaigns_active ON campaign_instances(active_start_date, active_end_date);
CREATE INDEX IF NOT EXISTS idx_contact_campaigns_lookup ON contact_campaigns(contact_id, campaign_instance_id);
CREATE INDEX IF NOT EXISTS idx_schedules_run ON email_schedules(scheduled_send_date);

-- Add missing columns to email_schedules if they don't exist
ALTER TABLE email_schedules ADD COLUMN priority INTEGER DEFAULT 10;
ALTER TABLE email_schedules ADD COLUMN campaign_instance_id INTEGER;
ALTER TABLE email_schedules ADD COLUMN email_template TEXT;
ALTER TABLE email_schedules ADD COLUMN sms_template TEXT;
ALTER TABLE email_schedules ADD COLUMN scheduler_run_id TEXT;

-- Insert built-in campaign types
INSERT OR REPLACE INTO campaign_types 
(name, respect_exclusion_windows, enable_followups, days_before_event, target_all_contacts, priority, active) 
VALUES 
('rate_increase', 1, 1, 14, 0, 1, 1),
('seasonal_promo', 1, 1, 7, 0, 5, 1),
('initial_blast', 0, 0, 0, 1, 10, 1),
('regulatory_notice', 0, 0, 0, 1, 1, 1),
('policy_update', 1, 1, 7, 0, 3, 1);