library(mixdist)
sample <- "aeg"
title_adendum <- "AS_proj_no"
ex <- FALSE

if(sample=="hrfu") {
	file_name <- "Classes/Astro_Stat/hrfu_proj_kin/hrfu_proj_all_data.csv"
}

if(sample=="aeg") {
	file_name <- "Classes/Astro_Stat/AEG_condor_all_data.csv"
}

substrRight <- function(x, n){
	substr(x, nchar(x)-n+1, nchar(x))
}

fit_funct <- function() {

	if(sample=="hrfu") {
		my_file <- read.csv(file_name)
		output_file <- "r_fit_summ_hrfu"
		plot_name <- "r_fit_plot_hrfu"
		main_title <- "Hi-Res Follow-Up"
		if(fix_cond=="free") {
			output_file <- paste(output_file,"free",sep="_")
			plot_name <- paste(plot_name,"free",sep="_")
			sub_title <- "free means"
		}
		if(fix_cond=="fixed") {
			output_file <- paste(output_file,"fix",sep="_")
			plot_name <- paste(plot_name,"fix",sep="_")
			sub_title <- "fixed means"
		}
		if(just_no==TRUE) {
			output_file <- paste(output_file,"no",sep="_")
			plot_name <- paste(plot_name,"no",sep="_")
			main_title <- paste(main_title,"(only CEMP-no)",sep=" ")
		}
		if(just_s==TRUE) {
			output_file <- paste(output_file,"s",sep="_")
			plot_name <- paste(plot_name,"s",sep="_")
			main_title <- paste(main_title,"(only CEMP-s)",sep=" ")
		}
		if(fit_comps=="IOT") {
			mus <- c(-85,0,180)
			sigmas <- c(180,100,45)
			output_file <- paste(output_file,"IOT.txt",sep="_")
			plot_name <- paste(plot_name,"IOT.pdf",sep="_")
			sub_title <- paste(sub_title,"IOT components",sep=", ")
		}
		if(fit_comps=="IO") {
			mus <- c(-85,0)
			sigmas <- c(180,100)
			output_file <- paste(output_file,"IO.txt",sep="_")
			plot_name <- paste(plot_name,"IO.pdf",sep="_")
			sub_title <- paste(sub_title,"IO components ([Fe/H]<-2, Z_max>3 kpc)",sep=", ")
		}
		if(fit_comps=="IOTex") {
			mus <- c(-400,-85,0,180)
			sigmas <- c(100,180,100,45)
			output_file <- paste(output_file,"IOTex.txt",sep="_")
			plot_name <- paste(plot_name,"IOTex.pdf",sep="_")
			sub_title <- paste(sub_title,"IOTex components",sep=", ")
		}
		if(fit_comps=="IOex") {
			mus <- c(-400,-85,0)
			sigmas <- c(100,180,100)
			output_file <- paste(output_file,"IOex.txt",sep="_")
			plot_name <- paste(plot_name,"IOex.pdf",sep="_")
			sub_title <- paste(sub_title,"IOex components ([Fe/H]<-2, Z_max>3 kpc)",sep=", ")
		}
	}

	if(sample=="aeg") {
		my_file <- read.csv(file_name)
		output_file <- "r_fit_summ_aeg"
		plot_name <- "r_fit_plot_aeg"
		main_title <- "AEGIS"
		if(fix_cond=="free") {
			output_file <- paste(output_file,"free",sep="_")
			plot_name <- paste(plot_name,"free",sep="_")
			sub_title <- "free means"
		}
		if(just_no==TRUE) {
			output_file <- paste(output_file,"no",sep="_")
			plot_name <- paste(plot_name,"no",sep="_")
			main_title <- paste(main_title,"(only CEMP-no)",sep=" ")
		}
		if(just_s==TRUE) {
			output_file <- paste(output_file,"s",sep="_")
			plot_name <- paste(plot_name,"s",sep="_")
			main_title <- paste(main_title,"(only CEMP-s)",sep=" ")
		}
		if(fix_cond=="fixed") {
			output_file <- paste(output_file,"fix",sep="_")
			plot_name <- paste(plot_name,"fix",sep="_")
			sub_title <- "fixed means"
		}
		if(fit_comps=="IOT") {
			mus <- c(-85,0,180)
			sigmas <- c(180,100,45)
			output_file <- paste(output_file,"IOT.txt",sep="_")
			plot_name <- paste(plot_name,"IOT.pdf",sep="_")
			sub_title <- paste(sub_title,"IOT components",sep=", ")
		}
		if(fit_comps=="IO") {
			mus <- c(-85,0)
			sigmas <- c(180,100)
			output_file <- paste(output_file,"IO.txt",sep="_")
			plot_name <- paste(plot_name,"IO.pdf",sep="_")
			sub_title <- paste(sub_title,"IO components ([Fe/H]<-2, Z_max>3 kpc)",sep=", ")
		}
		if(fit_comps=="IOTex") {
			mus <- c(-400,-85,0,180)
			sigmas <- c(100,180,100,45)
			output_file <- paste(output_file,"IOTex.txt",sep="_")
			plot_name <- paste(plot_name,"IOTex.pdf",sep="_")
			sub_title <- paste(sub_title,"IOTex components",sep=", ")
		}
		if(fit_comps=="IOex") {
			mus <- c(-400,-85,0)
			sigmas <- c(100,180,100)
			output_file <- paste(output_file,"IOex.txt",sep="_")
			plot_name <- paste(plot_name,"IOex.pdf",sep="_")
			sub_title <- paste(sub_title,"IOex components ([Fe/H]<-2, Z_max>3 kpc)",sep=", ")
		}
	}
	
	edited_file <- my_file[my_file[,"KIN_IN_S"]==1,]

	if(fit_comps=="IO" | fit_comps=="IOex") {
		edited_file <- edited_file[edited_file[,feh_col] < -2,]
		edited_file <- edited_file[edited_file[,Zm_col] > 3,]
		if(just_no==TRUE) {
			edited_file <- edited_file[(edited_file[,method_col]=="CEMP-no"),]
			my_list <- edited_file[,col_name]
		} else if(just_s==TRUE) {
			edited_file <- edited_file[(edited_file[,method_col]=="CEMP-s"),]
			my_list <- edited_file[,col_name]
		} else {
			my_list <- edited_file[,col_name]
		}
	}

	if(fit_comps=="IOT" | fit_comps=="IOTex") {
		if(just_no==TRUE) {
			edited_file <- edited_file[(edited_file[,method_col]=="CEMP-no"),]
			my_list <- edited_file[,col_name]
		} else if(just_s==TRUE) {
			edited_file <- edited_file[(edited_file[,method_col]=="CEMP-s"),]
			my_list <- edited_file[,col_name]
		} else {
			my_list <- edited_file[,col_name]
		}
	}

	rev_list <- my_list[!(my_list %in% c("nan","NaN"))]
	crop_list <- rev_list[list_min<=rev_list & rev_list<=list_max]

	my_data <- mixgroup(crop_list,breaks=c(seq(list_min,list_max,bin_size)))
	my_data <- as.mixdata(my_data)

	my_params <- mixparam(mus,sigmas)
	if(fix_cond=="free") {
		my_fit <- mix(my_data,my_params,"norm")
	}
	if(fix_cond=="fixed") {
		if(fit_comps=="IOT") {
			my_fit <- mix(my_data,my_params,"norm",mixconstr(conmu="MFX",fixmu=c(TRUE,TRUE,TRUE)))
		}
		if (fit_comps=="IO") {
			my_fit <- mix(my_data,my_params,"norm",mixconstr(conmu="MFX",fixmu=c(TRUE,TRUE)))
		}
		if(fit_comps=="IOTex") {
			my_fit <- mix(my_data,my_params,"norm",mixconstr(conmu="MFX",fixmu=c(TRUE,TRUE,TRUE,TRUE)))
		}
		if (fit_comps=="IOex") {
			my_fit <- mix(my_data,my_params,"norm",mixconstr(conmu="MFX",fixmu=c(TRUE,TRUE,TRUE)))
		}
	}	
	
	return_list <- list(my_fit,my_data,crop_list,plot_name,main_title,sub_title)
	return(return_list)
}

if(sample=="hrfu") {

	# set starting conditions
	col_name <- "vTg"
	method_col <- "combined_class"
	Zm_col <- "staeckel_Z_max"
	feh_col <- "feh"

	list_min <- -600
	list_max <- 600
	bin_size <- 50
	
	fix_cond <- "free"
	fit_comps <- "IOT"
	just_no <- FALSE
	just_s <- FALSE
	
	num_pages <- 3
	plots_per_page <- 4
	
	if(ex==TRUE) {
		comp_vect <- c("IOTex","IOex")
	}
	if(ex==FALSE) {
		comp_vect <- c("IOT","IO")
	}
	
	#loop over every possible condition & make a fit for each set of conditions
	for(plot_type in c("norm","scaled")) {
		num=1
		for (s_cond in c(FALSE,TRUE)) {
			just_s <- s_cond
			for (no_cond in c(FALSE,TRUE)) {
				just_no <- no_cond			
				for (comps in comp_vect) {
					fit_comps <- comps
					for (cond in c("free","fixed")) {
						fix_cond <- cond
						if((no_cond==TRUE & s_cond==TRUE)==FALSE) {		
							returned_list <- fit_funct()				
							sub_fit <- returned_list[[1]]
							sub_data <- returned_list[[2]]
							sub_clist <- returned_list[[3]]
							pname <- returned_list[[4]]
							mtitle <- returned_list[[5]]
							stitle <- returned_list[[6]]

							fit_name <- paste(sample,"fit",num,sep="_")
							assign(fit_name,sub_fit)
							data_name <- paste(sample,"data",num,sep="_")
							assign(data_name,sub_data)
							crop_name <- paste(sample,"clist",num,sep="_")
							assign(crop_name,sub_clist)
							plot_name <- paste(sample,"pname",num,sep="_")
							assign(plot_name,pname)
							mtitle_name <- paste(sample,"mtitle",num,sep="_")
							assign(mtitle_name,mtitle)
							stitle_name <- paste(sample,"stitle",num,sep="_")
							assign(stitle_name,stitle)
					
							num <- num + 1
						}
					}
				}
			}	
		}
	
		num = num - 1
	
		current_plot <- 1
		
		for(page in 1:num_pages) {
		
			#summary(fit) gives basic mixdist fit info (final params, errors, etc.)
			#print this info to separate txt files
	
			txt_file_name <- paste("r_stuff/r_deconvs/hrfu/hfru_summary",title_adendum,page,".txt",sep="_")
			sink(txt_file_name)
		
			for(plot_num in current_plot:(current_plot+plots_per_page-1)) {
	
				fit_name <- paste(sample,"fit",plot_num,sep="_")
				mtitle_name <- paste(sample,"mtitle",plot_num,sep="_")
				stitle_name <- paste(sample,"stitle",plot_num,sep="_")
		
				cat("Title",get(mtitle_name),"\n")
				cat("Sub-Title: ",get(stitle_name),"\n")

				summary(get(fit_name))
				cat("\n--------------------------------------------------\n")

			}	
	
			sink()
			
			#make 1 set of plots using mixdist's fits (y-axis: probability density)
			#make 1 set of scaled-up plots (y-axis: counts)
			
			if(plot_type=="norm") {
				plot_file_name <- paste("r_stuff/r_deconvs/hrfu/hrfu_plots_norm",title_adendum,page,".pdf",sep="_")
			}
			if(plot_type=="scaled") {
				plot_file_name <- paste("r_stuff/r_deconvs/hrfu/hrfu_plots_scaled",title_adendum,page,".pdf",sep="_")
			}
		
			pdf(plot_file_name)
			
			#4 plots per page
			if(plots_per_page==4) {
				par(mfrow = c(2,2))
			}
			else {
				print("plot set-up currently only supports 4 plots per page in a 2x2 configuration")
			}
	
			#grab fits and params that were previously run
			for (plot_num in current_plot:(current_plot+plots_per_page-1)) {
	
				fit_name <- paste(sample,"fit",plot_num,sep="_")
				data_name <- paste(sample,"data",plot_num,sep="_")
				crop_name <- paste(sample,"clist",plot_num,sep="_")
				plot_name <- paste(sample,"pname",plot_num,sep="_")
				mtitle_name <- paste(sample,"mtitle",plot_num,sep="_")
				stitle_name <- paste(sample,"stitle",plot_num,sep="_")
			
				for (comp in 1:length(get(fit_name)[[1]][[1]])) {	
				
					pi_name <- paste("pi",comp,sep="_")		
					pi <- get(fit_name)[[1]][[1]][[comp]]
					assign(pi_name,pi)
					mu_name <- paste("mu",comp,sep="_")
					mu <- get(fit_name)[[1]][[2]][[comp]]
					assign(mu_name,mu)
					sigma_name <- paste("sigma",comp,sep="_")
					sigma <- get(fit_name)[[1]][[3]][[comp]]
					assign(sigma_name,sigma)
				
					h <- hist(get(crop_name),breaks=c(seq(list_min,list_max,bin_size)),plot=FALSE)
					cmax <- max(h$counts)
					nmax <- max(h$density)
				
					amp <- cmax/nmax					
				}
				
				#plotting directions for 2 components (IO)
				if(substrRight(get(plot_name),6)=="IO.pdf" |  substrRight(get(plot_name),8)=="IOex.pdf") {			
					#plot mixdist's mixdata
					if(plot_type=="norm") {
						plot(get(fit_name),xlab="v_phi")
						
#						the commented values below can be used to check mixdist's norm fits against r's dnorm function
# 						(lines should overlap)

# 						x <- seq(list_min, list_max, by=0.1)
# 						curve(pi_1*dnorm(x, mean=mu_1, sd=sigma_1),add=TRUE)
# 						curve(pi_2*dnorm(x, mean=mu_2, sd=sigma_2),add=TRUE)
# 						curve((pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + pi_2*dnorm(x, mean=mu_2, sd=sigma_2)),add=TRUE)
					
					}
					if(plot_type=="scaled") {
						#plot "scaled-up" fits (y-axis: counts)
						sh <- hist(get(crop_name),breaks=c(seq(list_min,list_max,bin_size)),lty=0,main="",xlab="v_phi",ylab="#")
						#use lty=0 and a lines function to get rid of inner hist borders
						lines(c(sh$breaks,max(sh$breaks)),c(0,sh$counts,0),type='S',col="blue")
						if(substrRight(get(plot_name),6)=="IO.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2)
							pts_y <- c(0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
						if(substrRight(get(plot_name),8)=="IOex.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve(amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2,mu_3)
							pts_y <- c(0,0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
					}
				}
						
				#plotting directions for 3 components (IOT)
				if(substrRight(get(plot_name),7)=="IOT.pdf" |  substrRight(get(plot_name),9)=="IOTex.pdf") {
					if(plot_type=="norm") {
						#plot mixdist's mixdata
						plot(get(fit_name),xlab="v_phi")
						
#						the commented values below can be used to check mixdist's norm fits against r's dnorm function
# 						(lines should overlap)

# 						x <- seq(list_min, list_max, by=0.1)
# 						curve(pi_1*dnorm(x, mean=mu_1, sd=sigma_1),add=TRUE)
# 						curve(pi_2*dnorm(x, mean=mu_2, sd=sigma_2),add=TRUE)
# 						curve(pi_3*dnorm(x, mean=mu_3, sd=sigma_3),add=TRUE)
# 						curve((pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + pi_3*dnorm(x, mean=mu_3, sd=sigma_3)),add=TRUE)
					}
					if(plot_type=="scaled") {
						#plot "scaled-up" fits (y-axis: counts)
						nh <- hist(get(crop_name),breaks=c(seq(list_min,list_max,bin_size)),lty=0,main="",xlab="v_phi",ylab="#")
						#use lty=0 and a lines function to get rid of inner hist borders
						lines(c(nh$breaks,max(nh$breaks)),c(0,nh$counts,0),type='S',col="blue")
						if(substrRight(get(plot_name),7)=="IOT.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve(amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2,mu_3)
							pts_y <- c(0,0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
						if(substrRight(get(plot_name),9)=="IOTex.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve(amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3),col="red",add=TRUE)
							curve(amp*pi_4*dnorm(x, mean=mu_4, sd=sigma_4),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3) + amp*pi_4*dnorm(x, mean=mu_4, sd=sigma_4)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2,mu_3,mu_4)
							pts_y <- c(0,0,0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
					}
				}
			
				title(main=get(mtitle_name),cex.main=0.75)
				mtext(get(stitle_name),cex=0.75)
			}
		
			current_plot <- current_plot + plots_per_page
	
			dev.off()
		}
	}
}

if(sample=="aeg") {

	# set starting conditions
	col_name <- "vTg"
	method_col <- "combined_class"
	Zm_col <- "staeckel_Z_max"
	feh_col <- "feh"

	list_min <- -600
	list_max <- 600
	bin_size <- 50
	
	fix_cond <- "free"
	fit_comps <- "IOT"
	just_no <- TRUE
	just_s <- FALSE
	
	num_pages <- 1
	plots_per_page <- 4
	
	if(ex==TRUE) {
		comp_vect <- c("IOTex","IOex")
	}
	if(ex==FALSE) {
		comp_vect <- c("IOT","IO")
	}
	
	#loop over every possible condition & make a fit for each set of conditions
	for(plot_type in c("norm","scaled")) {
		num=1
		
		for (comps in comp_vect) {
			fit_comps <- comps
			for (cond in c("free","fixed")) {
				fix_cond <- cond				
				returned_list <- fit_funct()				
				sub_fit <- returned_list[[1]]
				sub_data <- returned_list[[2]]
				sub_clist <- returned_list[[3]]
				pname <- returned_list[[4]]
				mtitle <- returned_list[[5]]
				stitle <- returned_list[[6]]

				fit_name <- paste(sample,"fit",num,sep="_")
				assign(fit_name,sub_fit)
				data_name <- paste(sample,"data",num,sep="_")
				assign(data_name,sub_data)
				crop_name <- paste(sample,"clist",num,sep="_")
				assign(crop_name,sub_clist)
				plot_name <- paste(sample,"pname",num,sep="_")
				assign(plot_name,pname)
				mtitle_name <- paste(sample,"mtitle",num,sep="_")
				assign(mtitle_name,mtitle)
				stitle_name <- paste(sample,"stitle",num,sep="_")
				assign(stitle_name,stitle)
		
				num <- num + 1		
			}
		}
	
		num = num - 1
		
		current_plot <- 1
	
		for(page in 1:num_pages) {
		
			#summary(fit) gives basic mixdist fit info (final params, errors, etc.)
			#print this info to separate txt files

			txt_file_name <- paste("r_stuff/r_deconvs/aeg/aeg_summary",title_adendum,page,".txt",sep="_")
			sink(txt_file_name)
		
			for(plot_num in current_plot:(current_plot+plots_per_page-1)) {
	
				fit_name <- paste(sample,"fit",plot_num,sep="_")
				mtitle_name <- paste(sample,"mtitle",plot_num,sep="_")
				stitle_name <- paste(sample,"stitle",plot_num,sep="_")
		
				cat("Title",get(mtitle_name),"\n")
				cat("Sub-Title: ",get(stitle_name),"\n")

				summary(get(fit_name))
				cat("\n--------------------------------------------------\n")

			}	
	
			sink()
			
			#make 1 set of plots using mixdist's fits (y-axis: probability density)
			#make 1 set of scaled-up plots (y-axis: counts)
			
			if(plot_type=="norm") {
				plot_file_name <- paste("r_stuff/r_deconvs/aeg/aeg_plots_norm",title_adendum,page,".pdf",sep="_")
			}
			if(plot_type=="scaled") {
				plot_file_name <- paste("r_stuff/r_deconvs/aeg/aeg_plots_scaled",title_adendum,page,".pdf",sep="_")
			}
		
			pdf(plot_file_name)
			
			#4 plots per page
			if(plots_per_page==4) {
				par(mfrow = c(2,2))
			}
			else {
				print("plot set-up currently only supports 4 plots per page in a 2x2 configuration")
			}
	
			#grab fits and params that were previously run
			for (plot_num in current_plot:(current_plot+plots_per_page-1)) {
	
				fit_name <- paste(sample,"fit",plot_num,sep="_")
				data_name <- paste(sample,"data",plot_num,sep="_")
				crop_name <- paste(sample,"clist",plot_num,sep="_")
				plot_name <- paste(sample,"pname",plot_num,sep="_")
				mtitle_name <- paste(sample,"mtitle",plot_num,sep="_")
				stitle_name <- paste(sample,"stitle",plot_num,sep="_")
			
				for (comp in 1:length(get(fit_name)[[1]][[1]])) {	
				
					pi_name <- paste("pi",comp,sep="_")		
					pi <- get(fit_name)[[1]][[1]][[comp]]
					assign(pi_name,pi)
					mu_name <- paste("mu",comp,sep="_")
					mu <- get(fit_name)[[1]][[2]][[comp]]
					assign(mu_name,mu)
					sigma_name <- paste("sigma",comp,sep="_")
					sigma <- get(fit_name)[[1]][[3]][[comp]]
					assign(sigma_name,sigma)
				
					h <- hist(get(crop_name),breaks=c(seq(list_min,list_max,bin_size)),plot=FALSE)
					cmax <- max(h$counts)
					nmax <- max(h$density)
				
					amp <- cmax/nmax					
				}
			
				#plotting directions for 2 components (IO)
				if(substrRight(get(plot_name),6)=="IO.pdf" |  substrRight(get(plot_name),8)=="IOex.pdf") {			
					#plot mixdist's mixdata
					if(plot_type=="norm") {
						plot(get(fit_name),xlab="v_phi")
						
#						the commented values below can be used to check mixdist's norm fits against r's dnorm function
# 						(lines should overlap)

# 						x <- seq(list_min, list_max, by=0.1)
# 						curve(pi_1*dnorm(x, mean=mu_1, sd=sigma_1),add=TRUE)
# 						curve(pi_2*dnorm(x, mean=mu_2, sd=sigma_2),add=TRUE)
# 						curve((pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + pi_2*dnorm(x, mean=mu_2, sd=sigma_2)),add=TRUE)
					
					}
					if(plot_type=="scaled") {
						#plot "scaled-up" fits (y-axis: counts)
						sh <- hist(get(crop_name),breaks=c(seq(list_min,list_max,bin_size)),lty=0,main="",xlab="v_phi",ylab="#")
						#use lty=0 and a lines function to get rid of inner hist borders
						lines(c(sh$breaks,max(sh$breaks)),c(0,sh$counts,0),type='S',col="blue")
						if(substrRight(get(plot_name),6)=="IO.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2)
							pts_y <- c(0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
						if(substrRight(get(plot_name),8)=="IOex.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve(amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2,mu_3)
							pts_y <- c(0,0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
					}
				}
			
				#plotting directions for 3 components (IOT)
				if(substrRight(get(plot_name),7)=="IOT.pdf" |  substrRight(get(plot_name),9)=="IOTex.pdf") {
					if(plot_type=="norm") {
						#plot mixdist's mixdata
						plot(get(fit_name),xlab="v_phi")
						
#						the commented values below can be used to check mixdist's norm fits against r's dnorm function
# 						(lines should overlap)

# 						x <- seq(list_min, list_max, by=0.1)
# 						curve(pi_1*dnorm(x, mean=mu_1, sd=sigma_1),add=TRUE)
# 						curve(pi_2*dnorm(x, mean=mu_2, sd=sigma_2),add=TRUE)
# 						curve(pi_3*dnorm(x, mean=mu_3, sd=sigma_3),add=TRUE)
# 						curve((pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + pi_3*dnorm(x, mean=mu_3, sd=sigma_3)),add=TRUE)
					}
					if(plot_type=="scaled") {
						#plot "scaled-up" fits (y-axis: counts)
						nh <- hist(get(crop_name),breaks=c(seq(list_min,list_max,bin_size)),lty=0,main="",xlab="v_phi",ylab="#")
						#use lty=0 and a lines function to get rid of inner hist borders
						lines(c(nh$breaks,max(nh$breaks)),c(0,nh$counts,0),type='S',col="blue")
						if(substrRight(get(plot_name),7)=="IOT.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve(amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2,mu_3)
							pts_y <- c(0,0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
						if(substrRight(get(plot_name),9)=="IOTex.pdf") {
							curve(amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1),col="red",add=TRUE)
							curve(amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2),col="red",add=TRUE)
							curve(amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3),col="red",add=TRUE)
							curve(amp*pi_4*dnorm(x, mean=mu_4, sd=sigma_4),col="red",add=TRUE)
							curve((amp*pi_1*dnorm(x, mean=mu_1, sd=sigma_1) + amp*pi_2*dnorm(x, mean=mu_2, sd=sigma_2) + amp*pi_3*dnorm(x, mean=mu_3, sd=sigma_3) + amp*pi_4*dnorm(x, mean=mu_4, sd=sigma_4)),col="darkgreen",lwd=2,add=TRUE)
							pts_x <- c(mu_1,mu_2,mu_3,mu_4)
							pts_y <- c(0,0,0,0)
							points(pts_x,pts_y,col="red",bg="red",pch=24)
						}
					}
				}
			
				title(main=get(mtitle_name),cex.main=0.75)
				mtext(get(stitle_name),cex=0.75)
			}
		
			current_plot <- current_plot + plots_per_page
	
			dev.off()
		}
	}
}
