CREATE OR REPLACE VIEW user_public_key_resolution AS
SELECT
  pk.public_key,
  pk.created_at,
  pk.expires_at,
  COALESCE(s.user_uuid, upk.user_uuid) AS user_uuid,
  CASE
    WHEN s.user_uuid IS NOT NULL THEN 'session'
    WHEN upk.user_uuid IS NOT NULL THEN 'user'
    ELSE 'unknown'
  END AS key_type
FROM public_keys pk
LEFT JOIN sessions s ON s.public_key = pk.public_key
LEFT JOIN users_public_keys upk ON upk.public_key = pk.public_key;

CREATE OR REPLACE VIEW user_policy_resolution AS
SELECT DISTINCT up.user_uuid, up.policy_uuid
FROM users_policies up
UNION
SELECT DISTINCT gu.user_uuid, gp.policy_uuid
FROM groups_users gu
JOIN groups_policies gp ON gu.group_uuid = gp.group_uuid;
