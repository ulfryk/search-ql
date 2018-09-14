import { Map } from 'immutable';
import { Maybe, None } from 'monet';

import { Expression } from './expression';
import { Match } from './match';
import { ValueType } from './value-type';

// IDefaultInput below is not used right now. Yet will be in future :)
export class NodeEvaluation<V> {

  public static fromLogic(l: ValueType, r: ValueType) {
    return (input: Map<string, string>, node: Expression) =>
      (value: boolean, matches: () => Maybe<Map<string, Match>>) =>
        new NodeEvaluation(
          [l, r].includes(ValueType.Phrase) ? ValueType.Phrase : ValueType.Boolean,
          value, matches, input, node);
  }

  public static fromEquality(l: ValueType, r: ValueType) {
    return (input: Map<string, string>, node: Expression) =>
      (value: () => boolean, matches: () => Maybe<Map<string, Match>> = None) =>
        new NodeEvaluation(
          l === ValueType.Text && r === ValueType.Phrase ? ValueType.Phrase : ValueType.Boolean,
          value,
          matches,
          input,
          node);
  }

  public static ofBoolean(
    input: Map<string, string>,
    node: Expression,
  ) {
    return (value: boolean, matches: () => Maybe<Map<string, Match>> = None) =>
      new NodeEvaluation(ValueType.Boolean, value, matches, input, node);
  }

  public static ofNumber(
    input: Map<string, string>,
    node: Expression,
  ) {
    return (value: number, matches: () => Maybe<Map<string, Match>> = None) =>
      new NodeEvaluation(ValueType.Number, value, matches, input, node);
  }

  public static ofText(
    input: Map<string, string>,
    node: Expression,
  ) {
    return (value: string, matches: () => Maybe<Map<string, Match>> = None) =>
      new NodeEvaluation(ValueType.Text, value, matches, input, node);
  }

  public static ofPhrase(
    input: Map<string, string>,
    node: Expression,
  ) {
    return (matches: Maybe<Map<string, Match>>) =>
      new NodeEvaluation(ValueType.Phrase, matches.isSome(), () => matches, input, node);
  }

  public static ofPhraseLazy(
    input: Map<string, string>,
    node: Expression,
  ) {
    return (value: boolean, matches: () => Maybe<Map<string, Match>>) =>
      new NodeEvaluation(ValueType.Phrase, value, matches, input, node);
  }

  public static ofDate(
    input: Map<string, string>,
    node: Expression,
  ) {
    return (value: number, matches: () => Maybe<Map<string, Match>> = None) =>
      new NodeEvaluation(ValueType.Date, value, matches, input, node);
  }

  constructor(
    public readonly type: ValueType,
    public readonly value: V,
    public readonly matches: () => Maybe<Map<string, Match>>,
    public readonly input: Map<string, string>,
    public readonly node: Expression, // maybe lazy getter would be better ?
  ) {}

  public toString() {
    // TEMPORARY !!!
    return `NodeEvaluation[${this.type}] { value: ${this.value}, match: ${this.matches()} }`;
  }

  public inspect() {
    return this.toString();
  }

}
