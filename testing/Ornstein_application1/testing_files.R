# Testing to see if the hand_labels and recall includes all the info from the CEDA file

load('fuzzylink/testing/Ornstein_application1/hand_labels.RData')
load('fuzzylink/testing/Ornstein_application1/recall.RData')
load('fuzzylink/testing/Ornstein_application1/ceda.RData') # CEDA

#remove duplicates (mainly for hand_labels)
hand_label_unique <- hand_labels[!duplicated(hand_labels$A), ]
recall_unique <- recall[!duplicated(recall$A), ]

# combine the two DFs
combined <- full_join(hand_label_unique, recall_unique, by = "A")

# visualize how many A names were shared between the two DFs
shared_A <- intersect(hand_labels$A, recall$A)
cat(length(shared_A))


save(combined,
     file = paste0('fuzzylink/testing/Ornstein_application1/recall_and_hand.RData'))
save(shared_A,
     file = paste0('fuzzylink/testing/Ornstein_application1/shared_recall_and_hand.RData'))