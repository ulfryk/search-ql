import { Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Match } from '../match';
import { Expression } from './expression';

export class NotExpression extends Expression {

  public static of(value: Expression) {
    return new NotExpression(value);
  }

  constructor(
    public readonly value: Expression,
  ) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof NotExpression &&
      this.value.equals(other.value));
  }

  public toString() {
    return `NOT ${this.value.toString()}`;
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return this.value.test(values).cata(
      () => Some(values.map(text => Match.empty(text)).toMap()),
      () => None()); // tslint:disable-line:no-unnecessary-callback-wrapper
  }

}
