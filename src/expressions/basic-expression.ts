import { Map } from 'immutable';

import { matchBasicWord } from '../parsers/basic/match-basic-word';
import { Expression } from './expression';

export class BasicExpression extends Expression {

  public static fromMatch(match: string) {
    return new BasicExpression(match);
  }

  public readonly value: string;

  constructor(value: string) {
    super();
    this.value = value.toLowerCase();
  }

  public equals(other: Expression): boolean {
    return this === other || (other instanceof BasicExpression && this.value === other.value);
  }

  public toString() {
    return matchBasicWord(this.value)
      .map(([value]) => value)
      .filter(value => value === this.value)
      .orJust(`"${this.value}"`);
  }

  public test(values: Map<string, string>): boolean {
    return values.toSet().some(value => value.includes(this.value));
  }

}
