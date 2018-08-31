/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';
import { None, Some } from 'monet';

import { and, And, And0, config, like, Like0, not, Not, Not0, Or, or, Or0, txt } from '../testing/utils';
import { Expression } from './expressions';
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
      pairs(txt('aaa'), [Not0.token, txt('bbb')]),
      pairs(txt('aaa'), [Or0.token, txt('bbb')], [And.token, txt('ccc')]),
      pairs(txt('aaa'),
        [Or.token, fromPairs(
          pairs(txt('aaa'), [Or0.token, txt('bbb')], [And0.token, txt('ccc')]), config)],
        [And.token, txt('ccc')]),
      pairs(and(txt('aaa'), txt('bbb')), [Not.token, txt('ccc')]),
    ];

    const validOutput = [
      txt('aaa AND ) a:aaaa OR OR OR'),
      and(txt('aaa'), txt('bbb')),
      like(txt('first_name'), txt('John')),
      and(txt('aaa'), not(txt('bbb'))),
      or(txt('aaa'), and(txt('bbb'), txt('ccc'))),
      or(txt('aaa'), and(or(txt('aaa'), and(txt('bbb'), txt('ccc'))), txt('ccc'))),
      and(and(txt('aaa'), txt('bbb')), not(txt('ccc'))),
    ];

    zip<any>(validInput, validOutput).forEach(([input, output]) => {
      it(`should properly build expression for: ${output}`, () => {
        expect(fromPairs(input, config).equals(output)).to.be.true;
      });
    });

  });

});
