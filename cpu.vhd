-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Pavel Marek xmarek75
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

	---PC programovy citac
	signal pc_reg : std_logic_vector(11 downto 0); -- PC
	signal pc_inc : std_logic;          -- inkrementace citace
	signal pc_dec : std_logic;          -- dekrementace citace
	signal pc_clear : std_logic;        -- vynulovani citace

	---PTR ukazatel do pameti dat
	signal ptr_reg : std_logic_vector(9 downto 0); 
	signal ptr_inc : std_logic;         -- inkrementace ukazatele
	signal ptr_dec : std_logic;         -- dekrementace ukazatele
	signal ptr_clear: std_logic;        

	---CNT u while loop
	signal cnt_register : std_logic_vector(11 downto 0); -- citace poctu zavorek
	signal cnt_inc : std_logic;         -- inkrementace poctu zavorek
	signal cnt_dec : std_logic;         -- dekrementace poctu zavorek
	signal cnt_clear : std_logic;       


	signal mx_out : std_logic_vector(7 downto 0);
	signal mx_select : std_logic_vector(1 downto 0) := "00";


	type fsm_state is (
		s_start, --- stav start
		s_fetch, --- cteni instrukce
		s_decode, --- dekodovani

        s_program_inc, 
        s_program_mx_inc, 
        s_program_end_inc, 
        s_program_dec, 
        s_program_mx_dec, 
        s_program_end_dec, 

		s_pointer_inc, --- (>) - inkrementace ukazatele
		s_pointer_dec, --- (<) - dekrementace ukazatele
		
		s_while_end, --- ] konec whilu
        s_while_end1, 
        s_while_end2, 
        s_while_end3, 
        s_while_end_en, 
		s_while_start, ---zacatek whilu ]
        s_while1, 
        s_while2, 
        s_while_en, 
		
		s_write, 
        s_write1,
        s_write2,
        s_write_done, 
		s_get, 
        s_get_done, 
		s_break_start, 
        s_break1, 
        s_break_en, 
		s_null
	);
	signal state : fsm_state := s_start; ---aktualni stav
	signal nState : fsm_state; ---nasledujici stav

begin

	---programovy citac(PC).
	pc: process (CLK, RESET, pc_inc, pc_dec)
	begin
		if RESET = '1' then
			pc_reg <= (others => '0');
		elsif rising_edge(CLK) then
			if pc_inc = '1' then
				pc_reg <= pc_reg + 1;
			elsif pc_dec = '1' then
				pc_reg <= pc_reg - 1;
			elsif pc_clear = '1' then
				pc_reg <= (others => '0');
			end if;
		end if;
	end process;

	CODE_ADDR <= pc_reg;

    mx_logic: process (CLK, RESET, mx_select)
	begin
		if RESET = '1' then
			mx_out <= (others => '0');
		elsif rising_edge(CLK) then
			case mx_select is
				when "00" =>
					mx_out <= IN_DATA;
				when "01" =>
					mx_out <= DATA_RDATA + 1;

				when "10" =>
					mx_out <= DATA_RDATA - 1;
				when others =>
					mx_out <= (others => '0');
			end case;
		end if;
	end process;
	DATA_WDATA <= mx_out;

	cnt: process (CLK, RESET, cnt_inc, cnt_dec)
	begin
		if RESET = '1' then
			cnt_register <= (others => '0');
		elsif rising_edge(CLK) then
			if cnt_inc = '1' then
				cnt_register <= cnt_register + 1;
			elsif cnt_dec = '1' then
				cnt_register <= cnt_register - 1;
			elsif cnt_clear = '1' then
				cnt_register <= (others => '0');
			end if;
		end if;
	end process;
	OUT_DATA <= DATA_RDATA;
    
    ptr: process (CLK, RESET, ptr_inc, ptr_dec)
	begin
		if RESET = '1' then
			ptr_reg <= (others => '0');
		elsif rising_edge(CLK) then
			if ptr_inc = '1' then
				ptr_reg <= ptr_reg + 1;
			elsif ptr_dec = '1' then
				ptr_reg <= ptr_reg - 1;
			elsif ptr_clear = '1' then
				ptr_reg <= (others => '0');
			end if;
		end if;
	end process;
	DATA_ADDR <= ptr_reg;

	

	---state logic
	state_logic: process (CLK, RESET, EN)
	begin
		if RESET = '1' then
			state <= s_start;
		elsif rising_edge(CLK) then
			if EN = '1' then
				state <= nState;
			end if;
		end if;
	end process;

	---logika nasledujiciho stavu 
	nstate_logic: process (state, OUT_BUSY, IN_VLD, CODE_DATA, cnt_register, DATA_RDATA)
	begin
		---inicializace
		pc_inc <= '0';
		pc_dec <= '0';
		pc_clear <= '0';

		ptr_inc <= '0';
		ptr_dec <= '0';
		ptr_clear <= '0';

		cnt_inc <= '0';
		cnt_dec <= '0';
		cnt_clear <= '0';

		mx_select <= "00";

        CODE_EN <= '0';
		DATA_EN <= '0';
        OUT_WREN <= '0';
        DATA_WREN <= '0';
		IN_REQ <= '0';
		
		case state is
			--------pocatecni stav
			when s_start =>
				pc_clear <= '1'; -- PC = 0
				ptr_clear <= '1'; -- PTR = 0
				cnt_clear <= '1'; -- CNT = 0
				nState <= s_fetch;

			--------nacitani instrukce
			when s_fetch =>
				CODE_EN <= '1'; ---CODE_DATA=ROM[CODE_ADDR]
				nState <= s_decode;

			---------dekodovani
			when s_decode =>
				case CODE_DATA is
                    when X"00" =>
						nState <= s_null; --- null 
					when X"3E" =>
						nState <= s_pointer_inc; --- > inkrementace ukazatele
					when X"3C" =>
						nState <= s_pointer_dec; --- < dekrementace ukazatele
					when X"2B" =>
						nState <= s_program_inc; --- + inkrementace aktualni bunky
					when X"2D" =>
						nState <= s_program_dec; --- - dekrementace aktualni bunky
					when X"5B" =>
						nState <= s_while_start; --- [ zacatek whilu cyklu
					when X"5D" =>
						nState <= s_while_end; --- ] konec while ciklu
					when X"2E" =>
						nState <= s_write; --- . print hodnoty bunky
					when X"2C" =>
						nState <= s_get; --- , nacteni hodnoty do bunky
					when X"7E" =>
						nState <= s_break_start; --- ~ ukonceni while cyklu --break
                    when others =>
						pc_inc <= '1';
                        nState <= s_decode;
				end case;
                
            -------inkrementace pointru
			when s_pointer_inc => 
				pc_inc <= '1'; 
                ptr_inc <= '1'; 
				nState <= s_fetch;
			-------dekrementace pointru
			when s_pointer_dec =>
                pc_inc <= '1'; 
				ptr_dec <= '1'; 
				nState <= s_fetch;
			------inkrementace aktual bunky
			when s_program_inc =>
                DATA_WREN <= '0';
				DATA_EN <= '1';
				nState <= s_program_mx_inc;
			when s_program_mx_inc =>
				mx_select <= "01"; 
				nState <= s_program_end_inc;
			when s_program_end_inc =>
                pc_inc <= '1'; 
                DATA_WREN <= '1';
				DATA_EN <= '1';
				nState <= s_fetch;
			--------dekrementace aktual bunky
			when s_program_dec =>
				DATA_EN <= '1';
				DATA_WREN <= '0';
				nState <= s_program_mx_dec;

			when s_program_mx_dec =>
				mx_select <= "10"; 
				nState <= s_program_end_dec;

			when s_program_end_dec =>
                pc_inc <= '1'; 
                DATA_WREN <= '1';
				DATA_EN <= '1';				
				nState <= s_fetch;

			-------start while cyklu
			when s_while_start =>
                DATA_WREN <= '0';
				pc_inc <= '1'; 
				DATA_EN <= '1';
				nState <= s_while1;
			when s_while1 =>
				if DATA_RDATA /= (DATA_RDATA'range => '0') then 
					nState <= s_fetch;
				else ---if(DATA_RDATA==0)
					cnt_inc <= '1'; 
					CODE_EN <= '1'; 
					nState <= s_while2;
				end if;
			when s_while2 =>
				if cnt_register = (cnt_register'range => '0') then 
					nState <= s_fetch;
				else ---if(CNT!=0)
                    if CODE_DATA = X"5D" then ---if(CODE_DATA==']')
                        cnt_dec <= '1'; 
					elsif CODE_DATA = X"5B" then ---if(CODE_DATA=='[')
						cnt_inc <= '1'; 
					end if;
					pc_inc <= '1'; 
					nState <= s_while_en;
				end if;
			when s_while_en =>
				CODE_EN <= '1'; ---CODE_DATA=ROM[CODE_ADDR]
				nState <= s_while2;

			--------konec while cyklu
			when s_while_end =>
			    DATA_WREN <= '0';	
                DATA_EN <= '1';
				nState <= s_while_end1;
			when s_while_end1 =>
				if DATA_RDATA = (DATA_RDATA'range => '0') then ---if(DATA_RDATA==0)
					pc_inc <= '1'; 
					nState <= s_fetch;
				else 
					cnt_inc <= '1'; 
					pc_dec <= '1'; 
					nState <= s_while_end_en;
				end if;
			when s_while_end2 =>
				if cnt_register = (cnt_register'range => '0') then ---if(CNT == 0)
					nState <= s_fetch;
				else
					if CODE_DATA = X"5D" then ---if(CODE_DATA == ']')
						cnt_inc <= '1'; 
					elsif CODE_DATA = X"5B" then ---if(CODE_DATA == '[')
						cnt_dec <= '1'; 
					end if;
					nState <= s_while_end3;
				end if;
			when s_while_end3 =>
				if cnt_register = (cnt_register'range => '0') then ---if(CNT==0)
					pc_inc <= '1'; 
				else ---if(CNT!= 0)
					pc_dec <= '1'; 
				end if;
				nState <= s_while_end_en;
			when s_while_end_en =>
				CODE_EN <= '1'; ---CODE_DATA=ROM[CODE_ADDR]
				nState <= s_while_end2;
			-------write
            when s_write =>
                if OUT_BUSY = '0' then 
                    OUT_DATA <= DATA_RDATA;
                    OUT_WREN <= '1';
                    pc_inc <= '1';
                    nState <= s_fetch;
                end if;
			--------get
            when s_get =>
                mx_select <= "00";
                IN_REQ <= '1';
                nState <= s_get_done;
            when s_get_done =>
                if IN_VLD /= '1' then
                    mx_select <= "00";
                    IN_REQ <= '1';
                    nState <= s_get_done;
                else
                    DATA_EN <= '1';
                    DATA_WREN <= '1';
                    pc_inc <= '1'; 
                    nState <= s_fetch;
                end if;
			------break
			when s_break_start =>
                pc_inc <= '1';
				cnt_inc <= '1'; 
				nState <= s_break_en;
			when s_break1 =>
				if cnt_register = (cnt_register'range => '0') then 
					nState <= s_fetch;
				else 
                    if CODE_DATA = X"5D" then 
						cnt_dec <= '1'; 
					elsif CODE_DATA = X"5B" then 
						cnt_inc <= '1'; 
					end if;
					pc_inc <= '1'; 
					nState <= s_break_en;
				end if;
			when s_break_en =>
				CODE_EN <= '1'; 
				nState <= s_break1;
			---------null
			when s_null =>
				nState <= s_null;
			when others =>
				null;
		end case;
	end process;
end behavioral;