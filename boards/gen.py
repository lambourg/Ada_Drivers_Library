#!/usr/bin/env python

BOARDS = {'Crazyflie': ['ravenscar-sfp', 'ravenscar-full'],
          'MicroBit': ['zfp'],
          'Native': None,
          'OpenMV2': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F407Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F429Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F469Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F746Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F769Disco': ['ravenscar-sfp', 'ravenscar-full']}

def gen_project(board, rts):
    if board is None:
        project_name = 'Boards'
    elif rts is None:
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
    if board is None:
        cnt += '   type Board_Type is\n'
        boards = list(map(lambda x: x.lower(), BOARDS.keys()))
        cnt += '     ("%s");\n' % '",\n      "'.join(sorted(boards))
        cnt += '   Board : Board_Type := external("BOARD", "native");\n\n'
        cnt += '   type RTS_Type is ("zfp", "ravenscar-sfp", "ravenscar-full");\n'
        cnt += '   RTS : RTS_Type := external("RTS", "default");\n'
        cnt += '\n'

    elif BOARDS[board] is not None:
        lower = board.lower()
        cnt += '   Board := "%s";\n\n' % lower
        if rts is not None:
            cnt += '   RTS := "%s";\n' % rts
        elif len(BOARDS[board]) == 1:
            cnt += '   RTS := "%s";\n' % BOARDS[board][0]
        else:
            cnt += '   type RTS_Type is ("%s");\n' % '", "'.join(BOARDS[board])
            cnt += '   RTS : RTS_Type := external ("RTS", "%s");\n' % \
                   BOARDS[board][0]
        cnt += '\n'
    cnt += '   type Build_Type is ("Debug", "Production");\n'
    cnt += '   Build : Build_Type := external ("BUILD", "Production");\n'
    cnt += '\n'

    cnt += '   case Board is\n'
    cnt += '      when "native" =>\n'
    cnt += '      when others =>\n'
    cnt += '         for Target use "arm-eabi";\n'
    cnt += '   end case;\n'
    cnt += '\n'
    cnt += '   case Board is\n'
    cnt += '      when "native" =>\n'
    cnt += '      when "crazyflie" | "stm32f407disco" =>\n'
    cnt += '         for Runtime ("Ada") use RTS & "-stm32f4";\n'
    cnt += '      when others =>\n'
    cnt += '         for Runtime ("Ada") use RTS & "-" & Board;\n'
    cnt += '   end case;\n'
    cnt += '\n'
    cnt += '   Obj_Suffix := "";\n'
    cnt += '   case Board is\n'
    cnt += '      when "native" =>\n'
    cnt += '         Obj_Suffix := "native-" & Build;\n'
    cnt += '      when others =>\n'
    cnt += '         Obj_Suffix := RTS & "-" & Board & "-" & Build;\n'
    cnt += '   end case;\n'
    cnt += '\n'
    cnt += '   for Library_Name use Board;\n'
    cnt += '   for Library_Dir use "lib/" & Obj_Suffix;\n'
    cnt += '\n'
    cnt += '   for external ("Obj_Suffix") use Obj_Suffix;\n'
    if rts is not None:
        cnt += '   for external ("RTS") use RTS;\n'
    cnt += '   for Project_Files use (Board & "/board.gpr");\n'
    cnt += '\n'
    cnt += 'end %s;\n' % project_name

    print "creating %s.gpr" % project_name.lower()
    with open('%s.gpr' % project_name.lower(), 'w') as fp:
        fp.write(cnt)

if __name__ == "__main__":
    gen_project(None, None)
    for b in BOARDS:
        gen_project(b, None)
        if BOARDS[b] is not None and len(BOARDS[b]) > 1:
            for rts in BOARDS[b]:
                gen_project(b, rts)
