#!/usr/bin/env python

BOARDS = {'Crazyflie': ['ravenscar-sfp', 'ravenscar-full'],
          'MicroBit': ['zfp'],
          'Native': None,
          'RPi2': ['ravenscar-sfp'],
          'RPi3': ['ravenscar-sfp'],
          'OpenMV2': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F407Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F429Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F469Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F746Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F769Disco': ['ravenscar-sfp', 'ravenscar-full']}

def gen_project(board, rts):
    assert board is not None, "board is undefined"
    if rts is None:
        project_name = board
    else:
        if rts == 'zfp':
            suffix = 'ZFP'
        elif rts == 'ravenscar-sfp':
            suffix = 'SFP'
        elif rts == 'ravenscar-full':
            suffix = 'Full'
        else:
            assert False, "Unexpected runtime %s" % rts
        project_name = '%s_%s' % (board, suffix)

    cnt = 'aggregate library project %s is\n' % project_name
    cnt += '\n'
    lower = board.lower()
    cnt += '   Board := "%s";\n\n' % lower

    if BOARDS[board] is not None:
        if rts is not None:
            cnt += '   RTS := "%s";\n' % rts
        elif len(BOARDS[board]) == 1:
            cnt += '   RTS := "%s";\n' % BOARDS[board][0]
        else:
            cnt += '   type RTS_Type is ("%s");\n' % \
                   '", "'.join(BOARDS[board])
            cnt += '   RTS : RTS_Type := external ("RTS", "%s");\n' % \
                   BOARDS[board][0]
        cnt += '\n'

    cnt += '   type Build_Type is ("Debug", "Production");\n'
    cnt += '   Build : Build_Type := external ("BUILD", "Production");\n'
    cnt += '\n'

    if lower == 'native':
        pass
    elif lower == 'rpi3':
        cnt += '   for Target use "aarch64-elf";\n'
    else:
        cnt += '   for Target use "arm-eabi";\n'
    if lower == 'native':
        pass
    elif lower in ('crazyflie', 'stm32f407disco'):
        cnt += '   for Runtime ("Ada") use RTS & "-stm32f4";\n'
    else:
        cnt += '   for Runtime ("Ada") use RTS & "-" & Board;\n'
    cnt += '\n'
    if lower == 'native':
        cnt += '   Obj_Suffix := "native-" & Build;\n'
    else:
        cnt += '   Obj_Suffix := RTS & "-" & Board & "-" & Build;\n'
    cnt += '\n'
    cnt += '   for Library_Name use Board;\n'
    cnt += '   for Library_Dir use "lib/" & Obj_Suffix;\n'
    cnt += '\n'
    cnt += '   for external ("Obj_Suffix") use Obj_Suffix;\n'
    if rts is not None:
        cnt += '   for external ("RTS") use RTS;\n'
    if lower == 'rpi3':
        cnt += '   for Project_Files use ("rpi2/board.gpr");\n'
    else:
        cnt += '   for Project_Files use (Board & "/board.gpr");\n'
    cnt += '\n'
    cnt += 'end %s;\n' % project_name

    print "creating %s.gpr" % project_name.lower()
    with open('%s.gpr' % project_name.lower(), 'w') as fp:
        fp.write(cnt)

if __name__ == "__main__":
    for b in BOARDS:
        gen_project(b, None)
        if BOARDS[b] is not None and len(BOARDS[b]) > 1:
            for rts in BOARDS[b]:
                gen_project(b, rts)
