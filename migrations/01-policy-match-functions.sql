
-- Function to match a single path segment against a pattern
CREATE OR REPLACE FUNCTION match_path_segment(
  segment TEXT,
  pattern TEXT
) RETURNS BOOLEAN AS $$
BEGIN
  RETURN pattern = '*' OR segment = pattern;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to match double asterisk patterns
CREATE OR REPLACE FUNCTION match_double_asterisk(
  request_path TEXT,
  pattern TEXT
) RETURNS BOOLEAN AS $$
DECLARE
  pattern_parts TEXT[];
  request_parts TEXT[];
  i INTEGER;
  j INTEGER;
BEGIN
  pattern_parts := STRING_TO_ARRAY(pattern, '/');
  request_parts := STRING_TO_ARRAY(request_path, '/');

  i := 1;
  j := 1;

  WHILE i <= ARRAY_LENGTH(pattern_parts, 1) AND j <= ARRAY_LENGTH(request_parts, 1) LOOP
    IF pattern_parts[i] = '**' THEN
      IF i = ARRAY_LENGTH(pattern_parts, 1) THEN
        RETURN TRUE;  -- ** at the end matches everything
      ELSE
        i := i + 1;
        WHILE j <= ARRAY_LENGTH(request_parts, 1) AND NOT match_path_segment(request_parts[j], pattern_parts[i]) LOOP
          j := j + 1;
        END LOOP;
      END IF;
    ELSIF NOT match_path_segment(request_parts[j], pattern_parts[i]) THEN
      RETURN FALSE;
    END IF;

    i := i + 1;
    j := j + 1;
  END LOOP;

  RETURN i > ARRAY_LENGTH(pattern_parts, 1) AND j > ARRAY_LENGTH(request_parts, 1);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to match a request against a policy rule
CREATE OR REPLACE FUNCTION match_policy_rule(
  request_path TEXT,
  policy_resource TEXT
) RETURNS BOOLEAN AS $$
DECLARE
  pattern_parts TEXT[];
  request_parts TEXT[];
  i INTEGER;
BEGIN
  IF policy_resource LIKE '%**%' THEN
    RETURN match_double_asterisk(request_path, policy_resource);
  ELSE
    pattern_parts := STRING_TO_ARRAY(policy_resource, '/');
    request_parts := STRING_TO_ARRAY(request_path, '/');

    IF ARRAY_LENGTH(pattern_parts, 1) != ARRAY_LENGTH(request_parts, 1) THEN
      RETURN FALSE;
    END IF;

    FOR i IN 1..ARRAY_LENGTH(pattern_parts, 1) LOOP
      IF NOT match_path_segment(request_parts[i], pattern_parts[i]) THEN
        RETURN FALSE;
      END IF;
    END LOOP;

    RETURN TRUE;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to find matching policies for a given request
CREATE OR REPLACE FUNCTION find_matching_policies(
  request_path TEXT,
  is_write BOOLEAN,
  policy_uuids UUID[]
) RETURNS TABLE (policy_uuid UUID, rule_uuid UUID) AS $$
BEGIN
  RETURN QUERY
  SELECT pr.policy_uuid, pr.rule_uuid
  FROM policies_rules pr
  WHERE pr.policy_uuid = ANY(policy_uuids)
    AND ((is_write AND pr.write) OR (NOT is_write AND pr.read))
    AND match_policy_rule(request_path, pr.resource);
END;
$$ LANGUAGE plpgsql;
