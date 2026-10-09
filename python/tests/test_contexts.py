import pandas as pd
import numpy as np
import pytest
from tma import contexts, rules, conversation_rules
from tma.contexts import ContextModel


def make_df():
    """5 rows, 3 users, 2 conversations."""
    return pd.DataFrame({
        "user":  ["A", "B", "A", "C", "B"],
        "convo": ["c1", "c1", "c1", "c2", "c2"],
        "c1": [1, 0, 1, 0, 1],
        "c2": [0, 1, 0, 1, 0],
    })


def make_multimodal_df():
    """Multimodal data: chat + resource rows, two groups."""
    return pd.DataFrame({
        "userID":    ["U1", "U1", "U2", "U2", "U1", "U2"],
        "condition": ["A",  "A",  "A",  "A",  "A",  "A"],
        "chatGroup": ["G1", "G1", "G1", "G1", "G1", "G1"],
        "modality":  ["chat", "chat", "chat", "chat", "resource", "resource"],
        "c1": [1, 0, 0, 1, 1, 0],
        "c2": [0, 1, 1, 0, 0, 1],
    })


# ── Backwards-compatible conversations= interface ────────────────────────────

class TestContextsConversations:
    def test_returns_context_model(self):
        cm = contexts(make_df(), "user", "convo")
        assert isinstance(cm, ContextModel)

    def test_unit_labels(self):
        cm = contexts(make_df(), "user", "convo")
        assert set(cm.unit_labels) == {"A", "B", "C"}

    def test_unit_labels_first_appearance_order(self):
        cm = contexts(make_df(), "user", "convo")
        assert cm.unit_labels[0] == "A"

    def test_context_scoped_to_conversation(self):
        cm = contexts(make_df(), "user", "convo")
        # C only appears in c2 (rows 3, 4)
        assert set(cm.contexts_map["C"]) == {3, 4}

    def test_unit_own_rows(self):
        cm = contexts(make_df(), "user", "convo")
        assert cm.unit_own_rows["A"] == [0, 2]

    def test_multi_convo_unit_sees_both(self):
        cm = contexts(make_df(), "user", "convo")
        # B is in c1 (0,1,2) and c2 (3,4)
        assert set(cm.contexts_map["B"]) == {0, 1, 2, 3, 4}

    def test_no_conversations_no_hoo_rules_raises(self):
        with pytest.raises(ValueError, match="Provide at least one"):
            contexts(make_df(), "user")


# ── rules() helper ───────────────────────────────────────────────────────────

class TestRulesHelper:
    def test_rules_returns_list(self):
        r = rules(lambda df, u: df["user"] == u["user"])
        assert isinstance(r, list)
        assert len(r) == 1

    def test_rules_multiple(self):
        r = rules(
            lambda df, u: df["user"] == u["user"],
            lambda df, u: df["convo"] == "c1",
        )
        assert len(r) == 2

    def test_rules_are_callable(self):
        r = rules(lambda df, u: df["user"] == u["user"])
        assert callable(r[0])


# ── conversation_rules() helper ──────────────────────────────────────────────

class TestConversationRules:
    def test_returns_list_of_one(self):
        r = conversation_rules("convo")
        assert isinstance(r, list) and len(r) == 1

    def test_matches_same_convo(self):
        df = make_df()
        df["_unit_key"] = df["user"]
        df["_row_idx"]  = np.arange(len(df))
        unit_repr = df[df["user"] == "A"].iloc[0]
        r = conversation_rules("convo")
        mask = r[0](df, unit_repr)
        # A is in c1 → rows 0,1,2 should be True
        assert set(df.loc[mask, "_row_idx"].tolist()) == {0, 1, 2}

    def test_multi_col_match(self):
        df = pd.DataFrame({
            "user":      ["A", "A", "B", "B"],
            "condition": ["x", "x", "x", "y"],
            "convo":     ["c1","c1","c1","c1"],
        })
        df["_unit_key"] = df["user"] + "::" + df["condition"]
        df["_row_idx"]  = np.arange(len(df))
        unit_repr = df[df["user"] == "A"].iloc[0]  # condition=x
        r = conversation_rules("condition")
        mask = r[0](df, unit_repr)
        # Only rows with condition=x (rows 0,1,2)
        assert set(df.loc[mask, "_row_idx"].tolist()) == {0, 1, 2}

    def test_no_columns_raises(self):
        with pytest.raises(ValueError):
            conversation_rules()


# ── hoo_rules= interface ─────────────────────────────────────────────────────

class TestContextsHooRules:
    def test_explicit_lambda_same_as_conversations(self):
        df = make_df()
        cm_conv = contexts(df, "user", "convo")
        cm_hoo  = contexts(df, "user",
                           hoo_rules=conversation_rules("convo"))
        assert cm_conv.contexts_map == cm_hoo.contexts_map

    def test_rule_filters_by_column_value(self):
        df = make_df()
        # Unit-relative rule: each unit sees rows in its own conversation(s)
        hoo = rules(lambda df, u: df["convo"] == u["convo"])
        cm = contexts(df, "user", hoo_rules=hoo)
        # C is only in c2 → sees rows 3 and 4 (both in c2)
        assert set(cm.contexts_map["C"]) == {3, 4}

    def test_multi_rule_union(self):
        df = make_df()
        # Rule 1: same convo as unit; Rule 2: user == "B"
        hoo = rules(
            lambda df, u: df["convo"] == u["convo"],
            lambda df, u: df["user"]  == "B",
        )
        cm = contexts(df, "user", hoo_rules=hoo)
        # A is in c1; also sees all B rows (1, 4) via rule 2
        assert 1 in cm.contexts_map["A"]  # B's row in c1 (rule 1 gets this anyway)
        assert 4 in cm.contexts_map["A"]  # B's row in c2 (rule 2 adds this)

    def test_include_self_false(self):
        df = make_df()
        hoo = rules(lambda df, u: df["convo"] == u["convo"])
        cm = contexts(df, "user", hoo_rules=hoo, include_self=False)
        # A's own rows are 0 and 2 — include_self=False removes them
        # even though the rule (same convo) would have matched them
        assert 0 not in cm.contexts_map["A"]
        assert 2 not in cm.contexts_map["A"]
        assert 1 in cm.contexts_map["A"]  # B's row in c1 — other user, same convo

    def test_conversations_and_hoo_rules_combined(self):
        df = make_df()
        # conversations scopes to same convo; hoo_rules additionally adds all B rows
        cm = contexts(df, "user", conversations="convo",
                      hoo_rules=rules(lambda df, u: df["user"] == "B"))
        # A in c1 sees c1 rows (0,1,2) + all B rows (1,4) → {0,1,2,4}
        assert set(cm.contexts_map["A"]) == {0, 1, 2, 4}

    def test_multimodal_rules(self):
        """Mirrors R tma test pattern: separate chat and resource rules."""
        df = make_multimodal_df()
        hoo = rules(
            # chat rows from same group + condition
            lambda df, u: (df["modality"] == "chat")
                        & (df["chatGroup"] == u["chatGroup"])
                        & (df["condition"] == u["condition"]),
            # resource rows belonging to this specific user + condition
            lambda df, u: (df["modality"] == "resource")
                        & (df["userID"]    == u["userID"])
                        & (df["condition"] == u["condition"]),
        )
        cm = contexts(df, units=["userID", "condition"], hoo_rules=hoo)

        # U1 should see: all chat rows in G1/A (0,1,2,3) + U1 resource rows (4)
        u1_ctx = set(cm.contexts_map["U1::A"])
        assert {0, 1, 2, 3, 4}.issubset(u1_ctx)
        # U1 should NOT see U2's resource row (5) via resource rule
        assert 5 not in u1_ctx

        # U2 should see: all chat rows in G1/A (0,1,2,3) + U2 resource rows (5)
        u2_ctx = set(cm.contexts_map["U2::A"])
        assert {0, 1, 2, 3, 5}.issubset(u2_ctx)
        assert 4 not in u2_ctx
