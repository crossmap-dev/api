CREATE TABLE IF NOT EXISTS "users" (
  "uuid" UUID PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS "users_names" (
  "name" TEXT PRIMARY KEY,
  "user_uuid" UUID NOT NULL,
  FOREIGN KEY ("user_uuid") REFERENCES "users" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "users_public_keys" (
  "public_key" BYTEA PRIMARY KEY,
  "user_uuid" UUID NOT NULL,
  "created_at" TIMESTAMP NOT NULL,
  "expires_at" TIMESTAMP NOT NULL,
  FOREIGN KEY ("user_uuid") REFERENCES "users" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "groups" (
  "uuid" UUID PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS "groups_names" (
  "name" TEXT PRIMARY KEY,
  "group_uuid" UUID NOT NULL,
  FOREIGN KEY ("group_uuid") REFERENCES "groups" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "groups_users" (
  "group_uuid" UUID NOT NULL,
  "user_uuid" UUID NOT NULL,
  PRIMARY KEY ("group_uuid", "user_uuid"),
  FOREIGN KEY ("group_uuid") REFERENCES "groups" ("uuid") ON DELETE CASCADE,
  FOREIGN KEY ("user_uuid") REFERENCES "users" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "policies" (
  "uuid" UUID PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS "policies_names" (
  "name" TEXT PRIMARY KEY,
  "policy_uuid" UUID NOT NULL,
  FOREIGN KEY ("policy_uuid") REFERENCES "policies" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "policies_rules" (
  "policy_uuid" UUID NOT NULL,
  "rule_uuid" UUID NOT NULL,
  "read" BOOLEAN NOT NULL,
  "write" BOOLEAN NOT NULL,
  "resource" TEXT NOT NULL,
  PRIMARY KEY ("policy_uuid", "rule_uuid"),
  FOREIGN KEY ("policy_uuid") REFERENCES "policies" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "users_policies" (
  "user_uuid" UUID NOT NULL,
  "policy_uuid" UUID NOT NULL,
  PRIMARY KEY ("user_uuid", "policy_uuid"),
  FOREIGN KEY ("user_uuid") REFERENCES "users" ("uuid") ON DELETE CASCADE,
  FOREIGN KEY ("policy_uuid") REFERENCES "policies" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "groups_policies" (
  "group_uuid" UUID NOT NULL,
  "policy_uuid" UUID NOT NULL,
  PRIMARY KEY ("group_uuid", "policy_uuid"),
  FOREIGN KEY ("group_uuid") REFERENCES "groups" ("uuid") ON DELETE CASCADE,
  FOREIGN KEY ("policy_uuid") REFERENCES "policies" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "sessions" (
  "public_key" BYTEA PRIMARY KEY,
  "address" INET NOT NULL,
  "user_uuid" UUID NOT NULL,
  "created_at" TIMESTAMP NOT NULL,
  "expires_at" TIMESTAMP NOT NULL,
  FOREIGN KEY ("user_uuid") REFERENCES "users" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "sessions_policies" (
  "session_public_key" BYTEA NOT NULL,
  "policy_uuid" UUID NOT NULL,
  PRIMARY KEY ("session_public_key", "policy_uuid"),
  FOREIGN KEY ("session_public_key") REFERENCES "sessions" ("public_key") ON DELETE CASCADE,
  FOREIGN KEY ("policy_uuid") REFERENCES "policies" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "sessions_groups" (
  "session_public_key" BYTEA NOT NULL,
  "group_uuid" UUID NOT NULL,
  PRIMARY KEY ("session_public_key", "group_uuid"),
  FOREIGN KEY ("session_public_key") REFERENCES "sessions" ("public_key") ON DELETE CASCADE,
  FOREIGN KEY ("group_uuid") REFERENCES "groups" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "topics" (
  "uuid" UUID PRIMARY KEY,
  "broadcast" BOOLEAN NOT NULL,
  "log_events" BOOLEAN NOT NULL,
  "created_at" TIMESTAMP NOT NULL,
  "last_event_uuid" UUID
);

CREATE TABLE IF NOT EXISTS "topics_names" (
  "name" TEXT PRIMARY KEY,
  "topic_uuid" UUID NOT NULL,
  FOREIGN KEY ("topic_uuid") REFERENCES "topics" ("uuid") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "events" (
  "uuid" UUID PRIMARY KEY,
  "previous_event_uuid" UUID,
  "topic_uuid" UUID NOT NULL,
  "created_at" TIMESTAMP NOT NULL,
  "payload" JSONB NOT NULL,
  FOREIGN KEY ("topic_uuid") REFERENCES "topics" ("uuid") ON DELETE CASCADE
);
