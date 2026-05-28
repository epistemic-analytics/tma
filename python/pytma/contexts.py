"""pytma.contexts — Context model and HOO (History of Others) rules.

The primary API mirrors R tma's contexts() + rules() / conversation_rules():

    from pytma import contexts, rules, conversation_rules

    # Explicit rules — each callable receives the full DataFrame and the
    # current unit's representative row (a pandas Series), and must return
    # a boolean Series aligned with df.index.
    hoo = rules(
        lambda df, unit: (df["Condition"] == unit["Condition"])
                       & (df["GroupName"] == unit["GroupName"]),
    )
    cm = contexts(data, units=["Condition", "UserName"], hoo_rules=hoo)

    # Shorthand for same-column matching across one or more columns:
    hoo = conversation_rules("Condition", "GroupName")
    cm = contexts(data, units=["Condition", "UserName"], hoo_rules=hoo)

    # Convenience: pass a conversations= column directly (backwards compat)
    cm = contexts(data, units="user", conversations="convo")

Rule semantics (matching R tma):
    Each rule returns a boolean mask.  A row is visible to a unit if it
    matches ANY of the supplied rules (union / OR across rules).
"""
from __future__ import annotations

import numpy as np
import pandas as pd
from typing import Callable, Optional, Union


# ---------------------------------------------------------------------------
# Rule helpers
# ---------------------------------------------------------------------------

def rules(*callables: Callable[[pd.DataFrame, pd.Series], pd.Series]) -> list:
    """Bundle one or more HOO rule callables into a list.

    Each callable must have signature::

        def rule(df: pd.DataFrame, unit: pd.Series) -> pd.Series[bool]:
            ...

    where *df* is the full data (with internal ``_unit_key`` / ``_row_idx``
    columns present) and *unit* is the representative row for the current
    unit of analysis (equivalent to R's ``UNIT`` object).

    Example
    -------
    >>> hoo = rules(
    ...     lambda df, unit: df["Condition"] == unit["Condition"],
    ...     lambda df, unit: df["modality"].isin(["chat"]),
    ... )
    """
    return list(callables)


def conversation_rules(*cols: str) -> list:
    """Shorthand: rows that share the same value(s) as the unit in *cols*.

    Equivalent to R's::

        conversation_rules(ColA == UNIT$ColA & ColB == UNIT$ColB)

    Example
    -------
    >>> hoo = conversation_rules("Condition", "GroupName")
    >>> cm  = contexts(data, units=["Condition","UserName"], hoo_rules=hoo)
    """
    col_list = list(cols)
    if not col_list:
        raise ValueError("conversation_rules() requires at least one column name")

    def _rule(df: pd.DataFrame, unit: pd.Series) -> pd.Series:
        mask = pd.Series(True, index=df.index)
        for col in col_list:
            mask &= df[col] == unit[col]
        return mask

    return [_rule]


# ---------------------------------------------------------------------------
# contexts()
# ---------------------------------------------------------------------------

def contexts(
    data: pd.DataFrame,
    units: Union[str, list[str]],
    conversations: Optional[Union[str, list[str]]] = None,
    *,
    hoo_rules: Optional[list[Callable]] = None,
    include_self: bool = True,
) -> "ContextModel":
    """Build a context model defining each unit's visible rows.

    For each unit of analysis, the context is the set of row indices that
    unit is allowed to "see" during accumulation.  Rows are included if
    they match **any** of the supplied rules (union / OR semantics, matching
    R tma's behaviour of calling ``hoo()`` once per rule).

    Parameters
    ----------
    data : pd.DataFrame
    units : str | list[str]
        Column(s) whose combined values identify a unit of analysis.
    conversations : str | list[str] | None
        Convenience shorthand.  If provided (and *hoo_rules* is None),
        a unit's context is all rows that share the same value(s) in these
        column(s) — identical to ``hoo_rules=conversation_rules(*conversations)``.
        If *hoo_rules* is also provided, the conversation rule is ORed with
        the explicit rules.
    hoo_rules : list[callable] | None
        Explicit HOO rules.  Each callable has signature::

            rule(df: pd.DataFrame, unit: pd.Series) -> pd.Series[bool]

        Use :func:`rules` to bundle multiple callables, or
        :func:`conversation_rules` for the common same-column case.
        Rows matched by *any* rule are included in the context (union).
    include_self : bool
        If True (default), a unit's own rows are always included in its
        context regardless of rules.

    Returns
    -------
    ContextModel

    Raises
    ------
    ValueError
        If neither *conversations* nor *hoo_rules* is provided.

    Examples
    --------
    Simple conversation-based scoping (backwards-compatible)::

        cm = contexts(data, units="user", conversations="convo")

    Equivalent using explicit rule::

        cm = contexts(data, units="user",
                      hoo_rules=conversation_rules("convo"))

    Multi-modal rules (like R tma's test_mockdata pattern)::

        hoo = rules(
            lambda df, u: (df["modality"] == "chat")
                        & (df["chatGroup"] == u["chatGroup"])
                        & (df["condition"] == u["condition"]),
            lambda df, u: (df["modality"] == "resource")
                        & (df["userID"]    == u["userID"])
                        & (df["condition"] == u["condition"]),
        )
        cm = contexts(data, units=["userID","condition"], hoo_rules=hoo)
    """
    if conversations is None and hoo_rules is None:
        raise ValueError(
            "Provide at least one of: conversations= or hoo_rules=. "
            "Use conversation_rules() to build hoo_rules from column names."
        )

    units_cols = [units] if isinstance(units, str) else list(units)

    # Build the combined rule list
    active_rules: list[Callable] = []

    if conversations is not None:
        conv_cols = (
            [conversations] if isinstance(conversations, str) else list(conversations)
        )
        active_rules.extend(conversation_rules(*conv_cols))

    if hoo_rules is not None:
        active_rules.extend(hoo_rules)

    # Prepare working DataFrame
    df = data.copy().reset_index(drop=True)
    df["_unit_key"] = df[units_cols].astype(str).agg("::".join, axis=1)
    df["_row_idx"]  = np.arange(len(df))

    unit_labels: list[str] = list(dict.fromkeys(df["_unit_key"].tolist()))

    contexts_map:  dict[str, list[int]] = {}
    unit_own_rows: dict[str, list[int]] = {}

    for unit_label in unit_labels:
        own_mask = df["_unit_key"] == unit_label
        own_rows = df.loc[own_mask, "_row_idx"].tolist()
        unit_own_rows[unit_label] = own_rows

        # Apply each rule once per own row, unioning results.
        # This handles units that span multiple conversations: each own row
        # acts as the UNIT representative, so the rule is evaluated from
        # every "perspective" the unit occupies — equivalent to R tma calling
        # hoo() once per rule with the full unit context.
        visible: set[int] = set()
        for own_row_idx in own_rows:
            unit_repr: pd.Series = df.loc[own_row_idx]
            for rule in active_rules:
                mask = rule(df, unit_repr)
                visible.update(df.loc[mask, "_row_idx"].tolist())

        if include_self:
            # Guarantee own rows are present even if no rule matched them
            visible.update(own_rows)
        else:
            # Explicitly exclude own rows even if a rule matched them
            visible -= set(own_rows)

        contexts_map[unit_label] = sorted(visible)

    return ContextModel(
        data=df,
        unit_labels=unit_labels,
        unit_cols=units_cols,
        contexts_map=contexts_map,
        unit_own_rows=unit_own_rows,
    )


# ---------------------------------------------------------------------------
# ContextModel
# ---------------------------------------------------------------------------

class ContextModel:
    """Result of :func:`contexts`: maps each unit to its visible row indices.

    Attributes
    ----------
    data : pd.DataFrame
        Input data with added internal columns ``_unit_key``, ``_row_idx``.
    unit_labels : list[str]
        Unit labels in first-appearance order.
    unit_cols : list[str]
        Column(s) used to identify units.
    contexts_map : dict[str, list[int]]
        ``unit_label`` → sorted list of 0-based row indices visible to
        that unit (its HOO context).
    unit_own_rows : dict[str, list[int]]
        ``unit_label`` → sorted list of row indices *belonging* to that
        unit (rows where it is the responder).
    """

    def __init__(
        self,
        data: pd.DataFrame,
        unit_labels: list[str],
        unit_cols: list[str],
        contexts_map: dict[str, list[int]],
        unit_own_rows: dict[str, list[int]],
    ) -> None:
        self.data          = data
        self.unit_labels   = unit_labels
        self.unit_cols     = unit_cols
        self.contexts_map  = contexts_map
        self.unit_own_rows = unit_own_rows

    def __repr__(self) -> str:
        return (
            f"<ContextModel {len(self.unit_labels)} units, "
            f"{len(self.data)} rows>"
        )
