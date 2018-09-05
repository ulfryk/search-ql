// https://stackoverflow.com/questions/13627308/add-st-nd-rd-and-th-ordinal-suffix-to-a-number
import { Maybe } from 'monet';

const SUFFIXES = ['th', 'st', 'nd', 'rd'];
const TH = SUFFIXES[0];
const N100 = 100;
const N20 = 20;
const N10 = 10;

const getMaybe = (index: number) => Maybe.fromNull(SUFFIXES[index]);

const getSuffix = (mod100: number) =>
  getMaybe((mod100 - N20) % N10)
    .orElse(getMaybe(mod100))
    .orJust(TH);

export const toOrdinal = (n: number) => `${n}${getSuffix(Math.abs(n) % N100)}`;
