/* tslint:disable:no-unused-expression strict-boolean-expressions no-magic-numbers */
import { expect } from 'chai';
import * as P from 'parsimmon';

import { BasicExpression } from '../expressions';
import { exactMatch } from './exact-match';

describe('SearchQL parsers', () => {

  describe('exactMatch', () => {

    const validExactMatchInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'asdasd',
      '123123123',
    ];

    validExactMatchInput.forEach(input => {
      describe(`for '"${input}"'`, () => {
        const parsed = exactMatch.parse(`"${input}"`);

        it('should succeed', () => {
          expect(parsed.status).to.be.true;
        });

        it('should provide proper value', () => {
          expect(parsed.status ? parsed.value : null).to.deep.equal(new BasicExpression(input));
        });

      });
    });

    validExactMatchInput.concat('a "aa ').forEach(input => {
      describe(`for '${input}'`, () => {

        it('should fail', () => {
          expect(exactMatch.parse(input).status).to.be.false;
        });

      });
    });

    describe('for many valid occurrences', () => {
      const input = validExactMatchInput.map(__ => `"${__}"`).join(' ');
      const parser = P.sepBy1(exactMatch, P.whitespace);
      const expectedOutput = validExactMatchInput.map(BasicExpression.fromMatch);

      it('should succeed', () => {
        const output = parser.parse(input);
        expect(output.status).to.be.true;
      });

      it('should provide proper value', () => {
        const output = parser.parse(input);
        expect(output.status ? output.value : null).to.deep.equal(expectedOutput);
      });

    });

    describe('for mixed input (valid occurrences mixed with invalid strings)', () => {
      const input = validExactMatchInput.map((__, i) => i % 2 ? `"${__}"` : __).join(' ');
      const parser = P.sepBy1(exactMatch, P.whitespace);
      const output = parser.parse(input);

      it('should fail', () => {
        expect(output.status).to.be.false;
      });

    });

  });

});
