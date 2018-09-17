/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';
import { None, Some } from 'monet';

import { Expression, ValueType } from '../common/model';
import { and, And, And0, andNot, config, like, Like0, likeR, Not, Not0, notLike, NotLike0, notLikeR, Or, or, Or0, phrase, sel, txt } from '../testing/utils';
import { fromPairs } from './from-pairs';
import { OOPair } from './oopair';

const pairs = (initial: Expression, ...chain: [string, Expression][]) => [
  [None<string>(), initial],
  ...chain.map(([operator, rhs]) => [Some(operator), rhs]),
] as OOPair[];

describe('SearchQL ast', () => {

  describe('fromPairs', () => {

    const validInput = [
      pairs(txt('aaa AND ) a:aaaa OR OR OR')),
      pairs(txt('aaa'), [And0.token, txt('bbb')]),
      pairs(txt('first_name'), [Like0.token, txt('John')]),
      pairs(sel('first_name', ValueType.Text), [Like0.token, txt('John')]),
      pairs(txt('last_name'), [NotLike0.token, txt('John')]),
      pairs(sel('last_name', ValueType.Text), [NotLike0.token, txt('John')]),
      pairs(txt('aaa'), [Not0.token, txt('bbb')]),
      pairs(txt('aaa'), [Or0.token, txt('bbb')], [And.token, txt('ccc')]),
      pairs(txt('aaa'),
        [Or.token, fromPairs(
          pairs(txt('aaa'), [Or0.token, txt('bbb')], [And0.token, txt('ccc')]), config)],
        [And.token, txt('ccc')]),
      pairs(fromPairs(pairs(phrase('aaa'), [And0.token, phrase('bbb')]), config), [Not.token, txt('ccc')]),
    ];

    const validOutput = [
      txt('aaa AND ) a:aaaa OR OR OR'),
      and(phrase('aaa'), phrase('bbb')),
      likeR(txt('first_name'), txt('John')),
      like(txt('first_name'), txt('John')),
      notLikeR(txt('last_name'), txt('John')),
      notLike(txt('last_name'), txt('John')),
      andNot(phrase('aaa'), phrase('bbb')),
      or(phrase('aaa'), and(phrase('bbb'), phrase('ccc'))),
      or(phrase('aaa'), and(or(phrase('aaa'), and(phrase('bbb'), phrase('ccc'))), phrase('ccc'))),
      andNot(and(phrase('aaa'), phrase('bbb')), phrase('ccc')),
    ];

    zip<any>(validInput, validOutput)
      .map(([input, output]) => [fromPairs(input, config).reshape(), output])
      .forEach(([input, output]) => {
        it(`should properly build expression for: ${output}`, () => {
          expect(input.equals(output)).to.be.true;
        });
      });

  });

});
