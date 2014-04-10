/* A small program to read an original Redbrick House userfile and
   output an Erlang term suitable for use as a user_state record.
   Cf. src/user.hrl */
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* From RedbrickNUTS.h with non-userfiles data stripped. */
#define USER_NAME_LEN 12
#define USER_DESC_LEN 30
#define PHRASE_LEN 40
#define COLOURSTRING_LEN 10

struct user_struct {
  char capitalized[USER_NAME_LEN+1];
  int gender;
  int level;
  char desc[USER_DESC_LEN+1];
  char in_phrase[PHRASE_LEN+1];
  char out_phrase[PHRASE_LEN+1];
  char fencerank[USER_DESC_LEN+1];
  char houserank[USER_DESC_LEN+1];
  char colourstring[COLOURSTRING_LEN+1];
  char last_site[81];
  int last_login;
  int total_login_secs;
  int last_login_secs;
  int fencing_kills;

  /* Other elements of the original state we won't use. */
  int prompt;
  int muzzled;
  int charmode_echo;
  int command_mode;
  int colour;
  int ignsys;
  char email[USER_DESC_LEN+3];
};

void print(char *name, struct user_struct *u) {
  char *level;
  switch(u->level) {
    case 0: level = "user"; break;
    case 1: level = "tech"; break;
    case 2: level = "prof"; break;
  }

  puts("{ user_state,");
  printf("  \"%s\",\n", name);  
  printf("  %s,\n", u->gender ? "female" : "male");
  printf("  %s,\n", level);
  printf("  \"%s\",\n", u->capitalized);  
  printf("  \"%s\",\n", u->desc);  
  printf("  \"%s\",\n", u->in_phrase);  
  printf("  \"%s\",\n", u->out_phrase);  
  printf("  \"%s\",\n", u->fencerank);  
  printf("  \"%s\",\n", u->houserank);  
  printf("  \"%s\",\n", u->colourstring);  
  printf("  \"%s\",\n", u->last_site);  
  printf("  %d,\n", u->last_login);
  printf("  %d,\n", u->total_login_secs);
  printf("  %d,\n", u->last_login_secs);
  printf("  %d\n", u->fencing_kills);
  puts("}."); 
}

/* Awful code from RedbrickNUTS.c load_user_details(), minimal mods. */
struct user_struct *load(char *filename) {
  struct user_struct *u;
  FILE *f;
  char line[81];

  u = (struct user_struct *) calloc(1, sizeof(struct user_struct));
  if (NULL == u) {
    perror("allocating user_struct");
    exit(EXIT_FAILURE);
  }

  f = fopen(filename, "r");
  if (NULL == f) {
    perror("allocating user_struct");
    exit(EXIT_FAILURE);
  }

  fscanf(f, "%s", u->capitalized);
  fscanf(f, "%d %d %d %d %d %d %d %d %d %d %d %d",
         &u->last_login, &u->total_login_secs,
         &u->last_login_secs,
         &u->gender,
         &u->level,
         &u->prompt,
         &u->muzzled,
         &u->charmode_echo,
         &u->command_mode,
         &u->colour,
         &u->fencing_kills,
         &u->ignsys);
  fscanf(f, "%s\n", u->last_site);

  fgets(line, USER_DESC_LEN+2, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->desc, line); 

  fgets(line, PHRASE_LEN+2, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->in_phrase, line); 

  fgets(line, PHRASE_LEN+2, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->out_phrase, line); 

  fgets(line, USER_DESC_LEN+3, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->email, line);

  fgets(line, USER_DESC_LEN+1, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->fencerank, line);

  fgets(line, USER_DESC_LEN+1, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->houserank, line);

  fgets(line, USER_DESC_LEN+1, f);
  line[strlen(line) - 1] = 0;
  strcpy(u->colourstring, line);

  fclose(f);
  return u;
}

int main(int argc, char *argv[]) {
  struct user_struct *u;
  char filename[PATH_MAX];
  if (argc != 3) {
    printf("Usage: userfile_to_term <directory> <name>\n");
    exit(EXIT_FAILURE);
  }
  snprintf(filename, PATH_MAX, "%s/%s.D", argv[1], argv[2]);
  u = load(filename);
  print(argv[2], u);
  free(u);
  return EXIT_SUCCESS;
}
